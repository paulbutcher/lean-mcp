/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Json

public section

@[expose] section

namespace MCP

/-- Reads `k` from an object, `none` when the field is absent or `null`. -/
def optField? (j : Json) (k : String) (α : Type) [Json.FromJson α] :
    Except String (Option α) :=
  match j with
  | .obj fields =>
    match Json.findLast? fields k with
    | none => .ok none
    | some Json.null => .ok none
    | some v => (Option.some ·) <$> Json.fromJson? v
  | _ => .error "expected an object"

/-- Absent and present-but-null are different answers for a field whose value may itself be
`null`; `optField?` cannot tell them apart. -/
def optFieldRaw? (j : Json) (k : String) : Except String (Option Json) :=
  match j with
  | .obj fields => .ok (Json.findLast? fields k)
  | _ => .error "expected an object"

/-- Reads `k` from an object, failing when the field is absent. -/
def reqField? (j : Json) (k : String) (α : Type) [Json.FromJson α] : Except String α :=
  match j with
  | .obj fields =>
    match Json.findLast? fields k with
    | none => .error s!"missing field: {k}"
    | some v => Json.fromJson? v
  | _ => .error "expected an object"

/-- `optField?` where a malformed field is as good as an absent one. -/
def optOf? (j : Json) (k : String) (α : Type) [Json.FromJson α] : Option α :=
  match optField? j k α with
  | .ok v => v
  | .error _ => none

/-- Name and version of an MCP implementation, for display and logging only. -/
structure Implementation where
  name : String
  version : String
  title : Option String := none
deriving DecidableEq, Repr, Inhabited

def Implementation.toJson (i : Implementation) : Json :=
  Json.mkObj
    ([("name", Json.str i.name), ("version", Json.str i.version)] ++ Json.opt "title" i.title)

def Implementation.fromJson? (j : Json) : Except String Implementation := do
  return {
    name := ← reqField? j "name" String
    version := ← reqField? j "version" String
    title := ← optField? j "title" String
  }

/--
Hints about a tool's behaviour. Every field is advisory: a client is entitled to disbelieve all
of it, and the specification says as much.

This record is byte-identical across all four supported revisions, which is why the title of a
tool lives here rather than beside its name: `2025-03-26` has no top-level `title`, and every
revision that does still reads this one.
-/
structure ToolAnnotations where
  title : Option String := none
  readOnlyHint : Option Bool := none
  destructiveHint : Option Bool := none
  idempotentHint : Option Bool := none
  openWorldHint : Option Bool := none
deriving DecidableEq, Repr, Inhabited

def ToolAnnotations.isEmpty (a : ToolAnnotations) : Bool :=
  a.title.isNone && a.readOnlyHint.isNone && a.destructiveHint.isNone
    && a.idempotentHint.isNone && a.openWorldHint.isNone

def ToolAnnotations.toJson (a : ToolAnnotations) : Json :=
  Json.mkObj
    (Json.opt "title" a.title ++ Json.opt "readOnlyHint" a.readOnlyHint
      ++ Json.opt "destructiveHint" a.destructiveHint ++ Json.opt "idempotentHint" a.idempotentHint
      ++ Json.opt "openWorldHint" a.openWorldHint)

def ToolAnnotations.fromJson? (j : Json) : Except String ToolAnnotations := do
  return {
    title := ← optField? j "title" String
    readOnlyHint := ← optField? j "readOnlyHint" Bool
    destructiveHint := ← optField? j "destructiveHint" Bool
    idempotentHint := ← optField? j "idempotentHint" Bool
    openWorldHint := ← optField? j "openWorldHint" Bool
  }

/--
A tool as it appears in `tools/list`.

`inputSchema` and `outputSchema` are carried as JSON rather than a schema type: the protocol's
own requirement is that each be a valid JSON Schema object, and validating that is the
application's business.

A tool declaring an `outputSchema` should also return the data as a text block, for a client that
reads no `structuredContent`. That is the application's call; nothing here enforces it.
-/
structure Tool where
  name : String
  inputSchema : Json
  outputSchema : Option Json := none
  description : Option String := none
  annotations : ToolAnnotations := {}
deriving DecidableEq, Repr, Inhabited

/--
One encoding serves every supported revision: apart from `outputSchema`, the fields below are
declared, with the same names and the same types, by each of the four `schema.json` documents,
and none of the four requires a field this omits.

`outputSchema` arrived at `2025-06-18`, and is emitted whatever the revision in force. It is
absent unless an application sets one, and `Tool` is not closed to further properties, so gating
it would change a public signature to buy a `2025-03-26` client nothing it can observe.
-/
def Tool.toJson (t : Tool) : Json :=
  Json.mkObj
    ([("name", Json.str t.name)] ++ Json.opt "description" t.description
      ++ [("inputSchema", t.inputSchema)] ++ Json.opt "outputSchema" t.outputSchema
      ++ (if t.annotations.isEmpty then [] else [("annotations", t.annotations.toJson)]))

def Tool.fromJson? (j : Json) : Except String Tool := do
  let annotations ← optField? j "annotations" Json
  return {
    name := ← reqField? j "name" String
    inputSchema := ← reqField? j "inputSchema" Json
    outputSchema := ← optFieldRaw? j "outputSchema"
    description := ← optField? j "description" String
    annotations := ← match annotations with
      | none => .ok {}
      | some a => ToolAnnotations.fromJson? a
  }

/--
One block of a tool's unstructured result.

Only text is given a case of its own; `other` carries any of the blocks the specification
defines but this library does not model, so nothing on the wire is out of reach, and giving one
of them a case later takes nothing away from a caller that was building it by hand.
-/
inductive Content where
  | text (text : String)
  | other (block : Json)
deriving DecidableEq, Repr, Inhabited

def Content.toJson : Content → Json
  | .text s => Json.mkObj [("type", Json.str "text"), ("text", Json.str s)]
  | .other b => b

/--
What a tool returns.

A tool that failed returns this with `isError` set. It is not a JSON-RPC error: that is a
separate constructor of `Response`, reachable only when the call could not be dispatched at all.
-/
structure ToolResult where
  content : Array Content := #[]
  isError : Bool := false
  /--
  Any JSON value from `2026-07-28`, but an object in every handshake revision. A tool that may
  answer a client of either era should return an object here, or nothing.
  -/
  structuredContent : Option Json := none
deriving DecidableEq, Repr, Inhabited

/-- A result whose single block is `s`. -/
def ToolResult.text (s : String) : ToolResult := { content := #[.text s] }

/-- A failed call reported to the model, which may read `s` and try again. -/
def ToolResult.failure (s : String) : ToolResult := { content := #[.text s], isError := true }

end MCP

end
