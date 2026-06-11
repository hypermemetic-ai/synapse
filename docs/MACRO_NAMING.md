# Macro Naming — Current Canonical Reference

The Plexus macros went through a rename early in their life. Older architecture documents and tutorials sometimes reference the old names. This page is the canonical reference for what the macros are called today and what each does.

## Current canonical names

| Macro | Old name(s) | Purpose |
|---|---|---|
| `#[plexus_macros::activation(...)]` | `#[hub_methods]`, `#[hub_activation]` | Decorates an `impl` block to register the type as a Plexus activation. Carries `namespace`, `version`, `description`, `auth_posture`. |
| `#[plexus_macros::method(...)]` | `#[hub_method]` | Decorates an `async fn` inside an activation impl. Carries `description`, `params(...)`, optional posture overrides. |
| `#[plexus_macros::child(...)]` | (new — postdates the rename) | Declares a child router accessor on an activation. Generates the `ChildRouter` glue. |
| `#[derive(plexus_macros::Credentials)]` | (new — `AUTHZ-CRED-MACRO-1`) | Marks a return type whose fields include `Credential<T>` so the framework can advertise them in the method schema and route them through the dispatch sidecar. |
| `#[plexus_macros::from_auth(...)]` | (new) | Activation-resolver binding: pull a parameter from `AuthContext` at dispatch time. |

`auth_posture` on `#[activation(...)]` accepts:
- `"required"` — every method on the activation requires an authenticated caller.
- `"mixed"` — methods declare their own posture; the macro accepts both `auth: &AuthContext` (required) and `auth: Option<&AuthContext>` (optional, since `AUTHZ-MACRO-OPTIONAL-AUTH-1`).
- `"anonymous"` — every method is callable without auth; declaring `auth: &AuthContext` is a compile error.

## Migration recipe

```rust
// Old:
#[plexus_macros::hub_methods(namespace = "echo", version = "1.0.0", ...)]
impl Echo {
    #[plexus_macros::hub_method(description = "...", params(...))]
    async fn echo(&self, message: String) -> impl Stream<...> { ... }
}

// New:
#[plexus_macros::activation(namespace = "echo", version = "1.0.0", ...)]
impl Echo {
    #[plexus_macros::method(description = "...", params(...))]
    async fn echo(&self, message: String) -> impl Stream<...> { ... }
}
```

The wire protocol, schema emission, and dispatch behavior are unchanged across the rename — it was a naming cleanup, not a semantics change. Older docs that compare `hub_methods` to other macro systems (plexus-derive, etc.) describe the same behavior under the old name.

## Historical references

Some architecture documents in `docs/architecture/` were written before the rename. Where the old name appears, mentally substitute per the table above. The doc `16680731655417010687_hub-method-macro.md` carries an explicit superseded banner mapping every old name it uses.

## See also

- `plexus-macros/` source — the canonical implementation.
- `plans/AUTHZ/AUTHZ-CRED-MACRO-1.md`, `AUTHZ-CRED-MACRO-3.md`, `AUTHZ-MACRO-OPTIONAL-AUTH-1.md` — recent macro additions and fixes.
