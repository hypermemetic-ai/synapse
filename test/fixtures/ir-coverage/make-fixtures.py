# SYNAPSE-CC-8: the small fixtures of test/fixtures/ir-coverage, one per evidence row.
# Types copied from menger's 2026-10-29 schemas are taken from them verbatim.
import json, gzip, sys, os
out = sys.argv[1]
schemas = {}
with gzip.open(os.path.join(out, 'menger-2026-10-29.schemas.jsonl.gz')) as g:
    for line in g:
        r = json.loads(line); schemas['.'.join(r['path'])] = r['schema']

def defs_closure(ns, names):
    """The named $defs and everything they reference, from any method of ns."""
    pool = {}
    for m in schemas[ns]['methods']:
        for k in ('params', 'returns'):
            d = (m.get(k) or {}).get('$defs') or {}
            for n, v in d.items(): pool.setdefault(n, v)
    seen = {}
    todo = list(names)
    while todo:
        n = todo.pop()
        if n in seen: continue
        seen[n] = pool[n]
        def refs(v):
            if isinstance(v, dict):
                for k, x in v.items():
                    if k == '$ref': todo.append(x.split('/')[-1])
                    else: refs(x)
            elif isinstance(v, list):
                for x in v: refs(x)
        refs(pool[n])
    return dict(sorted(seen.items()))

def plugin(ns, methods, children=None):
    p = {"namespace": ns, "version": "1.0.0", "description": ns + " (fixture)", "hash": ns + "0", "methods": methods}
    if children: p["children"] = [{"namespace": c, "description": c, "hash": c + "0"} for c in children]
    return p

def event_method(name, field_types, defs, title="TicketsEvent"):
    """A method whose returns is an event union carrying one variant with these fields."""
    props = {"type": {"const": "got", "type": "string"}}
    props.update(field_types)
    return {"name": name, "description": name, "hash": name + "0", "role": {"kind": "rpc"},
            "returns": {"$schema": "https://json-schema.org/draft/2020-12/schema", "title": title,
                        "$defs": defs,
                        "oneOf": [{"properties": props, "required": list(props), "type": "object"}]}}

def write(name, rows):
    with open(os.path.join(out, name), 'w') as f:
        json.dump([{"path": p, "schema": s} for p, s in rows], f, indent=1, ensure_ascii=False, sort_keys=True)
        f.write('\n')

ref = lambda n: {"$ref": "#/$defs/" + n}

# a oneOf of an enum and consts is one string enum
d = defs_closure('tickets', ['TransitionField'])
write('transition-field.json', [(["tickets"], plugin("tickets", [event_method("history", {"field": ref("TransitionField")}, d)]))])
d = defs_closure('chats.inputs', ['AnswerSource'])
write('answer-source.json', [(["tickets"], plugin("tickets", [event_method("answers", {"source": ref("AnswerSource")}, d)]))])

# a union tagged kind
d = defs_closure('tickets', ['Reply'])
write('reply.json', [(["tickets"], plugin("tickets", [event_method("await_reply", {"reply": ref("Reply")}, d)]))])

# a union tagged event: the root's hash method, as menger sends it
root_hash = [m for m in schemas['']['methods'] if m['name'] == 'hash'][0]
write('hash-event.json', [(["tickets"], plugin("tickets", [root_hash]))])

# an externally tagged union: auth.login's flow
login = [m for m in schemas['auth']['methods'] if m['name'] == 'login'][0]
write('login-flow.json', [(["auth"], plugin("auth", [login]))])

# an untagged union of refs: what SYNAPSE-CC-9's schema method declares
sr_defs = {
  "PluginSchema": {"type": "object", "properties": {"namespace": {"type": "string"}, "methods": {"type": "array", "items": ref("MethodSchema")}}, "required": ["namespace", "methods"]},
  "MethodSchema": {"type": "object", "properties": {"name": {"type": "string"}, "params": {"description": "the params' JSON Schema: any JSON"}}, "required": ["name"]},
}
write('schema-result.json', [(["tickets"], plugin("tickets", [{"name": "schema", "description": "schema", "hash": "s0", "role": {"kind": "rpc"},
    "returns": {"title": "SchemaResult", "$defs": sr_defs, "anyOf": [ref("PluginSchema"), ref("MethodSchema")]}}]))])

# an unreadable union is refused, the type left out, its use RefUnknown
weird = {"Weird": {"oneOf": [{"type": "object", "properties": {"kind": {"const": "a", "type": "string"}, "x": {"type": "string"}}, "required": ["kind", "x"]}, {"type": "number"}]}}
write('unreadable-oneof.json', [(["tickets"], plugin("tickets", [event_method("weird", {"w": ref("Weird")}, weird)]))])

# a child's return is RefChild
write('static-child.json', [(["tickets"], plugin("tickets", [{"name": "inputs", "description": "the inputs child", "hash": "i0", "role": {"kind": "static_child"}}]))])

# an rpc method with no returns is refused
write('no-returns.json', [(["x"], plugin("x", [{"name": "schema", "description": "the schema", "hash": "s0", "role": {"kind": "rpc"}}]))])

# a struct root is a type: the root's hashes method, as menger sends it
root_hashes = [m for m in schemas['']['methods'] if m['name'] == 'hashes'][0]
write('hashes.json', [(["tickets"], plugin("tickets", [root_hashes]))])

# injected params leave mdParams
inj = {"name": "get", "description": "get", "hash": "g0", "role": {"kind": "rpc"},
       "params": {"type": "object", "required": ["ticket"], "properties": {
          "auth": {"x-plexus-source": {"from": "derived"}},
          "ticket": {"type": "string", "description": "The ticket"},
          "user": {"x-plexus-source": {"from": "auth", "resolver": "self.access.verify"}}}},
       "returns": {"title": "TicketsEvent", "oneOf": [{"type": "object", "properties": {"type": {"const": "ok", "type": "string"}}, "required": ["type"]}]}}
write('injected.json', [(["tickets"], plugin("tickets", [inj]))])

# a serde_json::Value is the JSON type: every spelling of it
js = {"Envelope": {"type": "object", "required": ["six", "described", "empty", "yes", "byref", "list", "map"], "properties": {
        "six": {"type": ["null", "boolean", "number", "string", "array", "object"], "description": "as plexus-core writes it (SYNAPSE-CC-9)"},
        "described": {"description": "the payload, annotations only"},
        "empty": {},
        "yes": True,
        "byref": {"$ref": "#/$defs/Json"},
        "list": {"type": "array", "items": {"type": ["object", "array", "string", "number", "boolean", "null"]}},
        "map": {"type": "object", "additionalProperties": True}}},
      "Json": {"type": ["null", "boolean", "number", "string", "array", "object"]}}
write('json.json', [(["tickets"], plugin("tickets", [event_method("call", {"envelope": ref("Envelope"), "value": {"description": "any value"}}, js)]))])

# what is still refused: a bare object, a bare array, a type list that is neither one type nor JSON
ut = {"Thing": {"type": "object", "required": ["bare", "list", "mixed"], "properties": {
        "bare": {"type": "object"},
        "list": {"type": "array"},
        "mixed": {"type": ["object", "boolean"]}}}}
write('untyped.json', [(["tickets"], plugin("tickets", [event_method("thing", {"thing": ref("Thing")}, ut)]))])

# a map, and an optional map: themes.Theme and themes.define, as menger sends them
define = [m for m in schemas['themes']['methods'] if m['name'] == 'define'][0]
d = defs_closure('themes', ['Theme'])
write('theme-map.json', [(["themes"], plugin("themes", [define, event_method("get", {"theme": ref("Theme")}, d, title="ThemesEvent")]))])

# a tuple: tickets.Component.preview
d = defs_closure('tickets.components', ['Component'])
write('component-preview.json', [(["tickets"], plugin("tickets", [event_method("components", {"component": ref("Component")}, d)]))])

# implied bounds carry nothing; others are refined
b = {"Bounds": {"type": "object", "required": ["a", "b", "c", "d", "e"], "properties": {
        "a": {"type": "integer", "format": "uint64", "minimum": 0},
        "b": {"type": "integer", "format": "uint16", "minimum": 0, "maximum": 65535},
        "c": {"type": "integer", "format": "uint64", "minimum": 1},
        "d": {"type": "string", "maxLength": 64},
        "e": {"type": ["integer", "null"], "format": "uint32", "minimum": 0}}}}
write('bounds.json', [(["tickets"], plugin("tickets", [event_method("bounds", {"bounds": ref("Bounds")}, b)]))])

# an unknown keyword is refused by name
ao = {"Both": {"allOf": [{"type": "object", "properties": {"a": {"type": "string"}}}, {"type": "object", "properties": {"b": {"type": "string"}}}]}}
write('allof.json', [(["tickets"], plugin("tickets", [event_method("both", {"both": ref("Both")}, ao)]))])
mo = {"Even": {"type": "object", "required": ["n"], "properties": {"n": {"type": "integer", "format": "int32", "multipleOf": 2}}}}
write('multipleof.json', [(["tickets"], plugin("tickets", [event_method("even", {"even": ref("Even")}, mo)]))])

# a variant field keeps its default
vd = {"name": "page", "description": "page", "hash": "p0", "role": {"kind": "rpc"},
      "returns": {"title": "TicketsEvent", "oneOf": [
          {"type": "object", "required": ["type"], "properties": {"type": {"const": "page", "type": "string"},
             "limit": {"type": "integer", "format": "uint32", "minimum": 0, "default": 1000}}},
          {"type": "object", "required": ["type", "message"], "properties": {"type": {"const": "error", "type": "string"}, "message": {"type": "string"}}}]}}
write('variant-default.json', [(["tickets"], plugin("tickets", [vd]))])

# mdInput (BIDIR-6), and its absence
tok = {"Token": {"type": "object", "required": ["text"], "properties": {"text": {"type": "string"}}}}
inp = dict(inj); inp = {"name": "chat", "description": "chat", "hash": "c0", "role": {"kind": "rpc"},
       "input": {"$ref": "#/$defs/Token", "$defs": tok},
       "returns": inj["returns"]}
noinp = {"name": "plain", "description": "plain", "hash": "p0", "role": {"kind": "rpc"}, "returns": inj["returns"]}
write('input.json', [(["tickets"], plugin("tickets", [inp, noinp]))])

# shared types are placed by references
ticket_ref = {"type": "string", "description": "A ticket's id, e.g. APP-21."}
def refs_method(name, n, tname="TicketRef", tdef=ticket_ref, title="Ev"):
    props = {"type": {"const": "e", "type": "string"}}
    for i in range(n): props["r%d" % i] = ref(tname)
    return {"name": name, "description": name, "hash": name + "0", "role": {"kind": "rpc"},
            "returns": {"title": title, "$defs": {tname: tdef}, "oneOf": [{"type": "object", "properties": props, "required": list(props)}]}}
pair = {"type": "object", "required": ["a"], "properties": {"a": {"type": "string"}}}
rows = [
  ([], plugin("service", [refs_method("ping", 3, "Shared", pair, "PingEvent")], ["notes", "tickets", "chats"])),
  (["chats"], plugin("chats", [refs_method("tie", 1, "Tied", pair, "ChatsEvent")])),
  (["notes"], plugin("notes", [refs_method("add", 1, title="NotesEvent")])),
  (["tickets"], plugin("tickets", [refs_method("get", 9, title="TicketsEvent"), refs_method("tie", 1, "Tied", pair, "TicketsEvent2"),
                                   refs_method("anc", 1, "Anc", pair, "TicketsEvent3"), refs_method("shared", 1, "Shared", pair, "TicketsEvent4")],
                     ["inputs"])),
  (["tickets", "inputs"], plugin("inputs", [refs_method("anc", 1, "Anc", pair, "InputsEvent")])),
]
write('shared.json', rows)
print("ok")
