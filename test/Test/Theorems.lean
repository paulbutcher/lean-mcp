/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import MCP

/-!
Claims about the protocol that hold for every input, rather than for the inputs a test happens
to name. Nothing here needs running: compiling is passing.
-/

namespace MCP.Test

/-- The value of field `k`, for reading back what an encoder produced. -/
def field? (j : Json) (k : String) : Option Json :=
  match j with
  | .obj fields => Json.findLast? fields k
  | _ => none

theorem version_ofString_toString (v : Version) : Version.ofString? v.toString = some v := by
  cases v <;> rfl

/-- A negotiated version is one of the four, because there is no fifth value to name. -/
theorem negotiate_supported (s : String) : Version.negotiate s ∈ Version.all := by
  unfold Version.negotiate
  cases h : Version.ofString? s with
  | none => decide
  | some v => cases v <;> decide

/-- A client naming a version this server speaks is answered with that version, not another. -/
theorem negotiate_exact (v : Version) : Version.negotiate v.toString = v := by
  cases v <;> rfl

/-- Every dispatched request is answered under its own id, whether it succeeded or failed. -/
theorem reply_carries_id (id : RequestId) (outcome : Except Failure Json) :
    (Reply.ofOutcome id outcome).body.id? = some id := by
  cases outcome <;> rfl

/-- A request that could not be read is answered with a null id, there being no other to use. -/
theorem unreadable_has_null_id (code : Int) (message : String) :
    (Reply.unreadable code message).body.id? = none := rfl

/--
A response encodes as a result or as an error and never as both, so a failed tool call and a
failed dispatch cannot be confused on the wire.
-/
theorem result_xor_error (r : Response) :
    (field? r.toJson "result").isSome = !(field? r.toJson "error").isSome := by
  cases r <;> rfl

-- Sixty-four leaf cases, each a field lookup that reduces through a string comparison.
set_option maxHeartbeats 400000 in
/-- A tool survives the journey out and back unchanged. -/
theorem tool_roundTrip (t : Tool) : Tool.fromJson? t.toJson = .ok t := by
  obtain ⟨name, schema, description, annotations⟩ := t
  obtain ⟨title, readOnly, destructive, idempotent, openWorld⟩ := annotations
  cases description <;> cases title <;> cases readOnly <;> cases destructive <;>
    cases idempotent <;> cases openWorld <;> rfl

/-- The `tools` of a `tools/list` result, each read back as a `Tool`. -/
def listedTools (j : Json) : Option (Array (Except String Tool)) :=
  match field? j "tools" with
  | some (.arr a) => some (a.map Tool.fromJson?)
  | _ => none

/-- `tools/list` lists each configured tool exactly once, in the order it was configured. -/
theorem tools_listed (cfg : Config) (v : Version) (info : Implementation) (ts : Array Tool) :
    listedTools (listToolsJson cfg v info ts) = some (ts.map (fun t => Except.ok t)) := by
  have h : (fun j => Tool.fromJson? j) ∘ Tool.toJson = fun t => Except.ok t :=
    funext tool_roundTrip
  cases v <;>
    simp [listedTools, field?, listToolsJson, resultEnvelope, Version.era, Json.mkObj,
      Json.findLast?, Array.map_map, h]

/-- Whatever a tool made of the call, its answer leaves as a result. -/
theorem toolResult_is_result (id : RequestId) (v : Version) (info : Implementation)
    (r : ToolResult) :
    (Reply.ofOutcome id (.ok (toolResultJson v info r))).body
      = .result id (toolResultJson v info r) := rfl

/-- A request that could not be dispatched leaves as an error, and only that way. -/
theorem failure_is_error (id : RequestId) (f : Failure) :
    (Reply.ofOutcome id (.error f)).body = .error (some id) f.err := rfl

/-- A failed tool call says so in the result, in either era. -/
theorem isError_survives (v : Version) (info : Implementation) (r : ToolResult) :
    field? (toolResultJson v info r) "isError" = some (Json.bool r.isError) := by
  obtain ⟨content, isError, structured⟩ := r
  cases v <;> cases structured <;> rfl

end MCP.Test
