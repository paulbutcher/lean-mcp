/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Json
public import MCP.Base64
public import MCP.Dispatch

public section

@[expose] section

namespace MCP

/--
The parts of an HTTP request the protocol reads.

Nothing else is needed, which is what keeps the core testable without an HTTP server: an
adapter over `Std.Http` has only to fill this in.
-/
structure HttpRequest where
  method : String
  origin : Option String := none
  /-- The `MCP-Protocol-Version` header. -/
  protocolVersion : Option String := none
  /-- The `Mcp-Method` header. -/
  mcpMethod : Option String := none
  /-- The `Mcp-Name` header, still in whatever form it arrived in. -/
  mcpName : Option String := none
  body : ByteArray := .empty

structure HttpResponse where
  status : Nat
  headers : Array (String × String) := #[]
  body : String := ""

/-- The `_meta` key on which the modern revision carries a request's protocol version. -/
def protocolVersionKey : String := "io.modelcontextprotocol/protocolVersion"

/-- The `_meta` key on which the modern revision carries the client's capabilities. -/
def clientCapabilitiesKey : String := "io.modelcontextprotocol/clientCapabilities"

def jsonResponse (status : Nat) (j : Json) : HttpResponse :=
  { status, headers := #[("Content-Type", "application/json")], body := j.compress }

def replyResponse (r : Reply) : HttpResponse := jsonResponse r.status r.body.toJson

/--
The transport requires this of both eras, and a connector directory requires it again: an
`Origin` that is present and not acceptable is forbidden, not merely undispatched.
-/
def originForbidden (origin : String) : HttpResponse :=
  jsonResponse 403
    (Response.error none
      { code := ErrorCode.invalidRequest,
        message := s!"Origin not allowed: {origin}" }).toJson

/--
No GET stream, and no session to delete. The modern revision removed both; the legacy one lets
a server decline them exactly this way.
-/
def methodNotAllowed : HttpResponse := { status := 405, headers := #[("Allow", "POST")] }

/-- A notification is answered with a status and nothing else. -/
def accepted : HttpResponse := { status := 202 }

/--
The `_meta` object of a request's params.

A `none` is "there is none", which every revision permits somewhere. An error is "there is one
and it is not an object", which is a different thing: a client that sent a malformed `_meta` has
not thereby sent no `_meta`, and reading the two the same way answers it as though it had.
-/
def requestMeta (params : Json) : Except String (Option Json) :=
  match params with
  | .obj fields =>
    match Json.findLast? fields "_meta" with
    | none => .ok none
    | some Json.null => .ok none
    | some j@(.obj _) => .ok (some j)
    | some _ => .error "_meta must be an object"
  | _ => .ok none

/-- The string at `key` of a request's `_meta`, absent and unreadable kept apart. -/
def metaString (params : Json) (key : String) : Except String (Option String) := do
  match ← requestMeta params with
  | none => .ok none
  | some metaObj =>
    match optField? metaObj key String with
    | .ok v => .ok v
    | .error _ => .error s!"_meta field must be a string: {key}"

/--
Whether `_meta` carries `key` at all, whatever its value.

A `_meta` that is not an object answers `false` here, but never reaches this: `resolveVersion`
refuses it first, and this runs only on a request whose version was resolved.
-/
def metaHas (params : Json) (key : String) : Bool :=
  match requestMeta params with
  | .ok (some (.obj fields)) => (Json.findLast? fields key).isSome
  | _ => false

def supportedVersion (s : String) : Except Failure Version :=
  match Version.ofString? s with
  | some v => .ok v
  | none => .error (Failure.unsupportedVersion s)

/--
Which revision a request is speaking.

The body's `_meta` is preferred over the header because it is the field the specification calls
the version "for this request"; where both are present they must agree, and where neither is
the transport allows a server that serves pre-`2025-06-18` clients to read `2025-03-26`, which
this one does. A `_meta` the version cannot be read out of is refused rather than treated as one
that named no version, which would answer from the header a request whose body disagreed.
-/
def resolveVersion (header : Option String) (fromMeta : Except String (Option String)) :
    Except Failure Version := do
  match ← fromMeta.mapError Failure.malformedMeta, header with
  | some s, some h =>
    if s == h then supportedVersion s
    else .error (Failure.headerMismatch
      s!"MCP-Protocol-Version header value '{h}' does not match body value '{s}'")
  | some s, none => do
    let v ← supportedVersion s
    match v.era with
    | .modern =>
      .error (Failure.headerMismatch "missing required header: MCP-Protocol-Version")
    | .legacy => .ok v
  | none, some h => supportedVersion h
  | none, none => .ok Version.headerDefault

/--
What the modern revision demands of a request beyond its content: the `_meta` fields it calls
required, and headers that agree with the body.
-/
def validateModern (req : HttpRequest) (r : Request) : Except Failure Unit := do
  if !metaHas r.params protocolVersionKey then
    .error (Failure.malformedMeta s!"missing required _meta field: {protocolVersionKey}")
  else if !metaHas r.params clientCapabilitiesKey then
    .error (Failure.malformedMeta s!"missing required _meta field: {clientCapabilitiesKey}")
  else
    match req.mcpMethod with
    | none => .error (Failure.headerMismatch "missing required header: Mcp-Method")
    | some hm =>
      if hm != r.method then
        .error (Failure.headerMismatch
          s!"Mcp-Method header value '{hm}' does not match body value '{r.method}'")
      else if r.method != "tools/call" then .ok ()
      else
        match req.mcpName with
        | none => .error (Failure.headerMismatch "missing required header: Mcp-Name")
        | some raw =>
          match Base64.decodeHeaderValue? raw with
          | none =>
            .error (Failure.headerMismatch
              "Mcp-Name header value is not valid Base64-encoded UTF-8")
          | some name =>
            match reqField? r.params "name" String with
            | .error _ => .ok ()
            | .ok bodyName =>
              if name == bodyName then .ok ()
              else
                .error (Failure.headerMismatch
                  s!"Mcp-Name header value '{name}' does not match body value '{bodyName}'")

/--
The whole transport, from the parts of a request that matter to a status, headers and a body.

An `initialize` selects legacy semantics for that exchange whatever the headers say, which is
what the specification means by an era being chosen by how the client opens.
-/
def serve [Monad m] (cfg : Config) (srv : Server m α) (caller : α) (req : HttpRequest) :
    m HttpResponse := do
  if let some origin := req.origin then
    if !cfg.allowOrigin origin then return originForbidden origin
  if req.method.toUpper != "POST" then return methodNotAllowed
  match Json.parseBytes req.body with
  | .error _ =>
    return replyResponse
      (Reply.unreadable ErrorCode.parseError "Parse error: the body is not valid JSON")
  | .ok body =>
    match Request.fromJson? body with
    | .error e => return replyResponse (Reply.unreadable ErrorCode.invalidRequest e)
    | .ok r =>
      match r.id with
      | none => return accepted
      | some id =>
        let resolved :=
          if r.method == "initialize" then .ok Version.headerDefault
          else resolveVersion req.protocolVersion (metaString r.params protocolVersionKey)
        match resolved with
        | .error f => return replyResponse (Reply.ofOutcome id (.error f))
        | .ok v =>
          let checked := match v.era with
            | .modern => validateModern req r
            | .legacy => .ok ()
          match checked with
          | .error f => return replyResponse (Reply.ofOutcome id (.error f))
          | .ok _ =>
            let outcome ← dispatch cfg srv caller v r
            return replyResponse (Reply.ofOutcome id outcome)

end MCP

end
