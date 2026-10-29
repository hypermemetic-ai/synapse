# Agent Plugin Architecture

## Overview

The Agent plugin provides stateful LLM conversations with persistent context. It combines:
- **Loom** for context tree storage (conversation history with branching)
- **cllient** for LLM API access (streaming completions)
- **Agent config** for identity and state (model, system prompt, current position)

The core abstraction is that an agent maintains a **position** in a conversation tree, and each interaction advances that position.

## Core Concept: Position

```
Position = (TreeId, NodeId)
```

A `Position` couples a tree reference with a node reference. This ensures:
1. You cannot reference a node without knowing its tree
2. The tree_id is immutable for a given agent (agents don't jump between trees)
3. Advancing position creates a new `Position` in the same tree

```rust
pub struct Position {
    pub tree_id: TreeId,
    pub node_id: NodeId,
}

impl Position {
    /// Move to a new node in the same tree
    pub fn advance(&self, new_node_id: NodeId) -> Self {
        Self {
            tree_id: self.tree_id,
            node_id: new_node_id,
        }
    }
}
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                              Service                                         │
│  ┌─────────────────────────────────────────────────────────────────┐    │
│  │                        Agent Plugin                              │    │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐  │    │
│  │  │   Storage   │  │  cllient    │  │     RPC Interface       │  │    │
│  │  │  (SQLite)   │  │ (LLM APIs)  │  │  create/get/list/chat   │  │    │
│  │  └──────┬──────┘  └─────────────┘  └─────────────────────────┘  │    │
│  │         │                                                        │    │
│  │         │ owns                                                   │    │
│  │         ▼                                                        │    │
│  │  ┌─────────────┐                                                 │    │
│  │  │    Loom     │  ← Agent's internal loom instance               │    │
│  │  │  (SQLite)   │                                                 │    │
│  │  └─────────────┘                                                 │    │
│  └─────────────────────────────────────────────────────────────────┘    │
│                                                                          │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐          │
│  │   Loom Plugin   │  │   Bash Plugin   │  │  Health Plugin  │          │
│  │   (separate)    │  │                 │  │                 │          │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Key Design Decision: Internal Loom

The Agent plugin **owns its own Loom instance** rather than calling the Loom plugin via RPC:

```rust
pub struct AgentPlugin {
    storage: Arc<AgentStorage>,      // Agent configs (SQLite)
    llm_registry: Arc<ModelRegistry>, // LLM client
}

pub struct AgentStorage {
    pool: SqlitePool,  // Agent table
    loom: LoomStorage, // Internal loom instance
}
```

**Why internal rather than RPC?**

1. **Atomicity**: Chat operations need to create nodes and update head atomically
2. **Performance**: No RPC overhead for internal operations
3. **Simplicity**: Direct method calls vs. constructing RPC requests
4. **Configuration sharing**: Same `LoomConfig` passed to both

**Trade-off**: The agent's trees are stored in the same database but are logically separate from any "public" Loom plugin. If both exist, they share the SQLite file but operate independently.

## Chat Flow

The `chat` method is the core operation:

```
┌──────────────────────────────────────────────────────────────────┐
│                         chat(agent_id, prompt)                    │
└──────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────┐
│ 1. Load agent config                                              │
│    - Get Position (tree_id, node_id)                              │
│    - Get model_id, system_prompt                                  │
└──────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────┐
│ 2. Build context from Loom                                        │
│    - loom.context_get_path(tree_id, node_id)                      │
│    - Returns [Node] from root to current position                 │
│    - Convert nodes to cllient Messages                            │
└──────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────┐
│ 3. Append user message to tree                                    │
│    - loom.node_create_text(tree_id, parent=node_id, "User: ...")  │
│    - Returns user_node_id                                         │
│    - Emit ChatStart { user_position }                             │
└──────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────┐
│ 4. Call LLM                                                       │
│    - Build request with context + prompt                          │
│    - Stream response via cllient                                  │
│    - Emit ChatContent { content } for each chunk                  │
│    - Accumulate full response                                     │
└──────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────┐
│ 5. Save assistant response                                        │
│    - loom.node_create_text(tree_id, parent=user_node_id,          │
│                            "Assistant: ...")                      │
│    - Returns response_node_id                                     │
└──────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────┐
│ 6. Update agent head                                              │
│    - agent.head = Position(tree_id, response_node_id)             │
│    - Emit ChatComplete { new_head, usage }                        │
└──────────────────────────────────────────────────────────────────┘
```

### Tree Structure After Chat

```
Tree created with agent:
└── (root)                          ← Initial head position
    └── User: Hello!                ← Created in step 3
        └── Assistant: Hi there!    ← Created in step 5, new head
            └── User: How are you?  ← Next chat, step 3
                └── Assistant: ...  ← Next chat, step 5, new head
```

## Message Role Convention

Nodes store role information in content prefixes:

| Prefix | Role |
|--------|------|
| `User: ` | User message |
| `Assistant: ` | Assistant response |
| `System: ` | System message |
| (none) | Treated as user |

This convention is simple but has limitations (see Risks).

## RPC Interface

| Method | Description |
|--------|-------------|
| `agent_create` | Create agent with new tree |
| `agent_get` | Get agent config |
| `agent_list` | List all agents |
| `agent_delete` | Delete agent (tree preserved) |
| `agent_chat` | Chat with streaming response |
| `agent_set_head` | Move to different position |

### Events

```rust
enum AgentEvent {
    AgentCreated { agent_id, head: Position },
    AgentData { agent: AgentConfig },
    AgentList { agents: Vec<AgentInfo> },
    AgentDeleted { agent_id },

    // Streaming chat events
    ChatStart { agent_id, user_position: Position },
    ChatContent { agent_id, content: String },
    ChatComplete { agent_id, new_head: Position, usage: Option<ChatUsage> },

    HeadUpdated { agent_id, old_head: Position, new_head: Position },
    Error { message: String },
}
```

## Concurrency Model

The plugin uses `Arc<AgentStorage>` without a mutex:

```rust
pub struct AgentPlugin {
    storage: Arc<AgentStorage>,  // No Mutex!
    llm_registry: Arc<ModelRegistry>,
}
```

**Why no mutex?**

1. `SqlitePool` handles connection pooling and thread safety internally
2. All storage methods are `async` and use SQLx which manages concurrency
3. Multiple chat streams can run concurrently without blocking each other

**SQLite considerations:**
- SQLite uses file-level locking
- Write operations are serialized at the database level
- Concurrent reads are allowed
- For high concurrency, consider WAL mode (Write-Ahead Logging)

## Risks and Limitations

### 1. Role Parsing Fragility

**Risk**: Message roles are determined by string prefixes (`User: `, `Assistant: `).

**Failure modes**:
- User message starting with "Assistant: " would be misinterpreted
- Content with no prefix defaults to user role
- No validation that roles alternate correctly

**Mitigation options**:
- Store role as separate field in node metadata
- Use structured node types instead of text prefixes
- Validate role sequence before sending to LLM

### 2. Head Update Race Condition

**Risk**: Between reading the head and updating it, another request could modify it.

```
Request A: read head = node_5
Request B: read head = node_5
Request A: create node_6, update head = node_6
Request B: create node_7 (parent = node_5!), update head = node_7
```

Result: node_7's parent is node_5, not node_6. The tree branches unexpectedly.

**Mitigation options**:
- Use optimistic locking (version field on agent)
- Use SQLite transactions with `BEGIN IMMEDIATE`
- Accept branching as a feature (it's a tree after all)

### 3. Context Window Overflow

**Risk**: Long conversations will exceed LLM context limits.

**Failure modes**:
- LLM returns error
- Truncation loses important context
- Cost increases with context size

**Mitigation options**:
- Implement context windowing (only send last N messages)
- Add summarization of older messages
- Track token count and warn before overflow

### 4. Orphaned Trees

**Risk**: Deleting an agent leaves its tree in the database.

**Design choice**: This is intentional—trees may be valuable historical data.

**Considerations**:
- Trees accumulate over time
- No automatic cleanup mechanism
- May want tree archival/deletion policy

### 5. LLM Failure Mid-Stream

**Risk**: LLM call fails after user node is created but before response is saved.

**State after failure**:
- User node exists in tree
- No assistant response node
- Head still points to previous position

**Recovery options**:
- Retry creates duplicate user nodes
- Could delete orphan user node on failure
- Could mark failed nodes specially

### 6. Internal vs External Loom Confusion

**Risk**: If both Agent plugin and Loom plugin are loaded, they share the same SQLite database but are logically separate.

**Potential confusion**:
- Trees created by Agent appear in Loom plugin queries
- External Loom operations could modify Agent trees
- No access control between them

**Mitigation options**:
- Use separate database files
- Add owner/namespace field to trees
- Document that Agent trees are "owned" by Agent

### 7. No Authentication/Authorization

**Risk**: Any caller can operate on any agent.

**Current state**: No auth mechanism—suitable for single-user/trusted environments.

**Future considerations**:
- Add owner field to agents
- Validate caller identity
- Scope operations to owner

## Future Enhancements

### Tool Execution

Agents could execute tools and store results as external handles:

```
└── User: List the files
    └── [bash:session-1/cmd-1]     ← External handle
        └── Assistant: I found 3 files...
```

This requires:
- Tool definition in agent config
- Tool execution during chat
- Handle creation and resolution

### Branching/Exploration

The `set_head` method enables branching:

```rust
// Go back to earlier point
agent.set_head(agent_id, earlier_node_id)

// Chat creates a branch
agent.chat(agent_id, "What if we tried X instead?")
```

### Multi-Agent Conversations

Multiple agents could share a tree:

```
└── Agent A: Let's discuss the problem
    └── Agent B: I think we should...
        └── Agent A: Good point, but...
```

Requires coordination on head positions.

## Configuration

```rust
pub struct AgentStorageConfig {
    /// Path to SQLite database for agent configs
    pub db_path: PathBuf,
    /// Loom configuration (agents use loom for context trees)
    pub loom_config: LoomConfig,
}
```

Both agent metadata and loom trees can use the same or different SQLite files depending on `LoomConfig`.

## Summary

The Agent plugin provides a stateful conversation abstraction over Loom trees. Key properties:

1. **Position-based state**: Agent tracks where it is in the conversation tree
2. **Internal Loom**: Direct programmatic access, not RPC
3. **Streaming chat**: Real-time token delivery during LLM calls
4. **Branching support**: `set_head` enables exploring alternate paths
5. **No mutex**: SqlitePool handles concurrency

Primary risks center around role parsing, race conditions, and context management. The design prioritizes simplicity and flexibility over strict safety guarantees.
