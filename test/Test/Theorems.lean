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

/--
The wire spelling of a revision parses back to that same revision. The server writes these
strings into an `initialize` reply, into `server/discover`'s `supportedVersions` and into the
`supported` list of an unsupported-version error, and a client that reads one back and sends it
must land where it started; the two tables cannot drift apart while this holds.

`v` ranges over `Version`, whose four constructors are the whole supported set, so no revision
is exempt. `Version.toString` is the date string the wire carries. `Version.ofString?` is the
parser, answering `some` on a string it recognises and `none` on anything else. Equality with
`some v` therefore says two things at once: the parse succeeds, and it yields the revision the
string was printed from rather than a neighbouring one.
-/
theorem version_ofString_toString (v : Version) : Version.ofString? v.toString = some v := by
  cases v <;> rfl

/--
Whatever a legacy client names in `initialize`, the version it is answered with is one this
server actually speaks. The legacy lifecycle requires a server that cannot meet the request to
name a revision it does support, which rules out echoing an unrecognised string back at the
client and leaving it to discover the problem later.

`s` is an arbitrary `String`, so the claim covers versions no revision uses and strings no
client would send, not just conformant input. `Version.negotiate` parses `s` and falls back to
`Version.latest`. `Version.all` is the list this server publishes as its supported set, and `∈`
is membership of it. The content of the claim is therefore that `all` names every constructor:
`Version` being closed makes any negotiated value a constructor, and an `all` that omitted one
would leave the server answering with a revision it never advertised.
-/
theorem negotiate_supported (s : String) : Version.negotiate s ∈ Version.all := by
  unfold Version.negotiate
  cases h : Version.ofString? s with
  | none => decide
  | some v => cases v <;> decide

/--
A client naming a revision this server speaks is answered with that revision and not another.
Without this the fallback could satisfy `negotiate_supported` by answering `latest` every time,
which is a supported answer and the wrong one: a client that asked for an older revision it can
handle would be told to speak one it cannot.

`v` ranges over the four supported revisions, and `v.toString` is what a client asking for one
of them puts in `params.protocolVersion`. `Version.negotiate` maps that string back. Equality
with `v` says the `getD Version.latest` fallback is not reached for any of them, so the fallback
applies only to strings outside the set. Not vacuous: `Version` has four inhabitants and the
proof discharges all four.
-/
theorem negotiate_exact (v : Version) : Version.negotiate v.toString = v := by
  cases v <;> rfl

/--
Every dispatched request is answered under the id it arrived with, whether the dispatch
succeeded or failed. JSON-RPC clients pair answers to requests by id alone, so an answer under
the wrong id, or under none, is one the client cannot match; the failure path is where that is
easiest to lose.

`id` is an arbitrary `RequestId`, so both the string and the integer form are covered.
`outcome` is the dispatcher's verdict, `.ok` carrying a result value or `.error` carrying a
`Failure`, and `Reply.ofOutcome` is the one place either becomes a `Response`. `Response.id?`
reads the id back out, answering `none` for an error that carries no id. `= some id` therefore
says that neither branch takes the id-less form, and that the id both produce is the one passed
in rather than one constructed along the way.
-/
theorem reply_carries_id (id : RequestId) (outcome : Except Failure Json) :
    (Reply.ofOutcome id outcome).body.id? = some id := by
  cases outcome <;> rfl

/--
The one answer that carries no id is the answer to a request whose id could not be read. JSON-RPC
permits a null id only there, and taken with `reply_carries_id` this splits the two paths
exactly: a request that reached the dispatcher is answered under its own id, and a request that
could not be parsed is the only thing answered without one.

`code` and `message` are arbitrary, so nothing about the particular error is doing the work; the
result would be the same for any code the server might choose. `Reply.unreadable` is the
constructor used when the body could not be parsed as JSON-RPC. `Response.id?` answers `none`
exactly on an `.error` whose id field is absent, so `= none` says this constructor always builds
that form, and never invents an id to fill the gap.
-/
theorem unreadable_has_null_id (code : Int) (message : String) :
    (Reply.unreadable code message).body.id? = none := rfl

/--
On the wire an answer carries a result or an error, never both and never neither. This is what
keeps a tool that failed distinguishable from a request that could not be dispatched: the first
is a result whose `ToolResult` has `isError` set, the second a JSON-RPC error, and a client
reading them can only tell them apart if exactly one member is ever present.

`r` is any `Response`, so both constructors and any payload either might hold. `field? j "result"`
answers `some` when the encoded object has a `result` member and `none` otherwise, and likewise
for `error`; `.isSome` is `true` exactly when the member is there. The equation states that the
two booleans are opposites, which is stronger than saying they differ in the cases anyone
happens to test: on every response, one member present forces the other absent, and each
constructor supplies one of them.
-/
theorem result_xor_error (r : Response) :
    (field? r.toJson "result").isSome = !(field? r.toJson "error").isSome := by
  cases r <;> rfl

-- Sixty-four leaf cases, each a field lookup that reduces through a string comparison.
set_option maxHeartbeats 400000 in
/--
A tool survives encoding and decoding unchanged, so what a client reads out of `tools/list` is
the tool the application configured, field for field. The one encoding serves all four
revisions, and a field dropped or misnamed on the way out would be invisible to a test that only
inspects the fields it thought to name.

`t` ranges over every `Tool`: an arbitrary `name`, an arbitrary `inputSchema`, a `description`
present or absent, and every combination of the five optional fields of `ToolAnnotations`,
including the empty annotations that `Tool.toJson` omits from the object entirely. `Tool.toJson`
is the encoder `tools/list` uses and `Tool.fromJson?` the decoder, which answers `.error` on
anything it cannot read. `.ok t` therefore says decoding both succeeds and returns the original
record, so no field is lost, renamed, or defaulted to something other than what it held.
-/
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

/--
`tools/list` offers every configured tool, once each, in the order it was configured, and does
so in both eras. The protocol requires a deterministic order, and the modern envelope wraps the
same list in fields the legacy one has no place for; this says that wrapping neither drops a
tool, nor repeats one, nor reorders them.

`ts` is any array of tools, so the empty array is one case among all rather than the case being
described; `cfg` and `info` are arbitrary, and `v` ranges over all four revisions, covering both
the modern envelope and the bare legacy object. `listToolsJson` builds the result and
`listedTools` reads it back, answering `some` only when the result has a `tools` member holding
an array, then decoding each element. Equality with `ts.map Except.ok` says that member is
present and is an array, that every element decodes without error, and that the decoded sequence
is `ts` in its original positions. That the decoded tools are the configured ones rests on
`tool_roundTrip`, which the proof uses to rewrite the decode of each encoded element.
-/
theorem tools_listed (cfg : Config) (v : Version) (info : Implementation) (ts : Array Tool) :
    listedTools (listToolsJson cfg v info ts) = some (ts.map (fun t => Except.ok t)) := by
  have h : (fun j => Tool.fromJson? j) ∘ Tool.toJson = fun t => Except.ok t :=
    funext tool_roundTrip
  cases v <;>
    simp [listedTools, field?, listToolsJson, resultEnvelope, Version.era, Json.mkObj,
      Json.findLast?, Array.map_map, h]

/--
Whatever a tool made of the call, its answer leaves as a JSON-RPC result and not a JSON-RPC
error. The two mean different things to a client: a `ToolResult` marked failed is something the
model may read and act on, whereas an error says the call was never dispatched at all. This
fixes the branch a completed call takes, and `failure_is_error` fixes the other.

`r` is any `ToolResult`, whether or not `isError` is set and whatever its content and
`structuredContent`; `v` is any revision, `info` any server identity, and `id` any request id.
`toolResultJson` encodes the tool's answer, and passing it through `.ok` to `Reply.ofOutcome` is
what the dispatcher does for every call that ran. The equation says the resulting `Response` is
the `.result` constructor, under `id`, carrying that encoding unaltered, and so is never the
`.error` constructor for any tool outcome.
-/
theorem toolResult_is_result (id : RequestId) (v : Version) (info : Implementation)
    (r : ToolResult) :
    (Reply.ofOutcome id (.ok (toolResultJson v info r))).body
      = .result id (toolResultJson v info r) := rfl

/--
A request that could not be dispatched leaves as a JSON-RPC error, and the error the dispatcher
built is the error that reaches the wire. The client's only account of what went wrong is the
code and message in that object, so a path that substituted a generic one would leave a caller
unable to tell a bad protocol version from an unknown method.

`f` is any `Failure`, so any transport status paired with any error object, including the codes
the specification reserves to itself. `.error f` is how the dispatcher reports a request it
could not answer at all. The equation says `Reply.ofOutcome` yields the `.error` constructor,
that its id is `some id` rather than the id-less form `Reply.unreadable` uses, and that its
payload is `f.err` itself, so the status in `f` is the only part of the failure the transport
consumes.
-/
theorem failure_is_error (id : RequestId) (f : Failure) :
    (Reply.ofOutcome id (.error f)).body = .error (some id) f.err := rfl

/--
A tool's own verdict on the call reaches the client as the tool gave it, in either era.
`isError` is the only thing that tells a model a call it made failed; dropped, it reads as a
success, and a model would go on to use an answer that is really an apology.

`r` is any `ToolResult` and `v` any of the four revisions, so this covers the modern envelope,
which surrounds these fields with `resultType` and `_meta`, as well as the bare legacy object.
`field? j "isError"` answers `some` when the encoded result carries that member and `none` when
it does not. `some (Json.bool r.isError)` therefore says the member is present in every case,
`false` included rather than being left to be inferred from its absence, and that the boolean it
holds is the one the tool set.
-/
theorem isError_survives (v : Version) (info : Implementation) (r : ToolResult) :
    field? (toolResultJson v info r) "isError" = some (Json.bool r.isError) := by
  obtain ⟨content, isError, structured⟩ := r
  cases v <;> cases structured <;> rfl

end MCP.Test
