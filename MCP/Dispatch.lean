/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Json
public import MCP.JsonRpc
public import MCP.Protocol
public import MCP.Server

public section

@[expose] section

namespace MCP

/-- What this server offers. Tools, and deliberately nothing else. -/
def serverCapabilities : Json := Json.mkObj [("tools", Json.mkObj [])]

namespace Failure

/-- An ordinary dispatch failure: a JSON-RPC error on a `200`, as JSON-RPC over HTTP expects. -/
def ofCode (code : Int) (message : String) (data : Option Json := none) : Failure :=
  { status := 200, err := { code, message, data } }

def invalidParams (message : String) : Failure := ofCode ErrorCode.invalidParams message

/--
The modern transport answers an unimplemented method with `404`, so that a client probing for
the era can tell a modern server from one that does not host the endpoint at all. The legacy
transport says nothing about it, and a JSON-RPC error on a `200` is what its clients expect.
-/
def methodNotFound (v : Version) (name : String) : Failure :=
  { status := match v.era with | .modern => 404 | .legacy => 200,
    err := { code := ErrorCode.methodNotFound, message := s!"Method not found: {name}" } }

def headerMismatch (message : String) : Failure :=
  { status := 400, err := { code := ErrorCode.headerMismatch, message } }

/-- A required `_meta` field is missing, which the base protocol makes a `400` and `-32602`. -/
def malformedMeta (message : String) : Failure :=
  { status := 400, err := { code := ErrorCode.invalidParams, message } }

def unsupportedVersion (requested : String) : Failure :=
  { status := 400,
    err := {
      code := ErrorCode.unsupportedProtocolVersion,
      message := s!"Unsupported protocol version: {requested}",
      data := some (Json.mkObj
        [("supported", Json.arr ((Version.allStrings.map Json.str).toArray)),
         ("requested", Json.str requested)]) } }

end Failure

/--
The fields every modern result carries and no legacy result may.

`resultType` is required from `2026-07-28`; `serverInfo` in `_meta` is a SHOULD that costs a
stateless server nothing, since it is the only place it can name itself.
-/
def resultEnvelope (v : Version) (info : Implementation) (fields : List (String × Json)) : Json :=
  match v.era with
  | .modern =>
    Json.mkObj
      ((("resultType", Json.str "complete") :: fields)
        ++ [("_meta", Json.mkObj [("io.modelcontextprotocol/serverInfo", info.toJson)])])
  | .legacy => Json.mkObj fields

/-- The result of `tools/list`, in the order the tools were configured. -/
def listToolsJson (cfg : Config) (v : Version) (info : Implementation) (tools : Array Tool) :
    Json :=
  let listed := [("tools", Json.arr (tools.map Tool.toJson))]
  match v.era with
  | .modern =>
    resultEnvelope v info
      (listed ++ [("ttlMs", Json.ofNat cfg.cacheTtlMs), ("cacheScope", cfg.cacheScope.toJson)])
  | .legacy => resultEnvelope v info listed

def toolResultJson (v : Version) (info : Implementation) (r : ToolResult) : Json :=
  resultEnvelope v info
    ([("content", Json.arr (r.content.map Content.toJson)),
      ("isError", Json.bool r.isError)]
      ++ Json.opt "structuredContent" r.structuredContent)

/--
The result of `server/discover`, always in the modern shape.

The method exists only in the modern revision, so there is no legacy shape it could take; a
client that asks for it is asking a modern question and is answered in modern terms.
-/
def discoverJson (cfg : Config) (info : Implementation) (instructions : Option String) : Json :=
  resultEnvelope .v2026_07_28 info
    ([("supportedVersions", Json.arr ((Version.allStrings.map Json.str).toArray)),
      ("capabilities", serverCapabilities),
      ("ttlMs", Json.ofNat cfg.cacheTtlMs),
      ("cacheScope", cfg.cacheScope.toJson)]
      ++ Json.opt "instructions" instructions)

/--
The result of a legacy `initialize`.

No `Mcp-Session-Id` accompanies this. The tools carry no per-connection state, so a session
would hold nothing, and declining to mint one keeps every request independent, which is what
lets the server sit behind a function URL that freezes between invocations.
-/
def initializeJson (info : Implementation) (instructions : Option String) (v : Version) : Json :=
  Json.mkObj
    ([("protocolVersion", Json.str v.toString),
      ("capabilities", serverCapabilities),
      ("serverInfo", info.toJson)]
      ++ Json.opt "instructions" instructions)

/-- The version an `initialize` is answered with, read from its `params.protocolVersion`. -/
def negotiateInitialize (params : Json) : Version :=
  match reqField? params "protocolVersion" String with
  | .ok s => Version.negotiate s
  | .error _ => Version.latest

def callTool [Monad m] (srv : Server m α) (caller : α) (v : Version) (params : Json) :
    m (Except Failure Json) := do
  match reqField? params "name" String with
  | .error e => return .error (Failure.invalidParams e)
  | .ok name =>
    match srv.find? name with
    | none => return .error (Failure.invalidParams s!"Unknown tool: {name}")
    | some td =>
      let arguments := optOf? params "arguments" Json
      let result ← td.run caller (arguments.getD (Json.obj #[]))
      return .ok (toolResultJson v srv.info result)

/--
Answers one request, given the version the transport determined it was speaking.

A tool that fails leaves by the `.ok` branch, carrying a `ToolResult` with `isError` set. Only a
request that could not be dispatched at all leaves by `.error`.
-/
def dispatch [Monad m] (cfg : Config) (srv : Server m α) (caller : α) (v : Version)
    (req : Request) : m (Except Failure Json) :=
  match req.method with
  | "tools/call" => callTool srv caller v req.params
  | "tools/list" =>
    pure (.ok (listToolsJson cfg v srv.info (srv.tools.map (·.tool))))
  | "server/discover" =>
    pure (.ok (discoverJson cfg srv.info srv.instructions))
  | "initialize" =>
    match v.era with
    | .legacy =>
      pure (.ok (initializeJson srv.info srv.instructions (negotiateInitialize req.params)))
    | .modern => pure (.error (Failure.methodNotFound v req.method))
  | other => pure (.error (Failure.methodNotFound v other))

end MCP

end
