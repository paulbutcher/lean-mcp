# MCP

Model Context Protocol server for Lean 4.

## Adding it to your project

```toml
[[require]]
name = "MCP"
git = "https://github.com/paulbutcher/lean-mcp"
```

## Usage

```lean
import MCP

open MCP

/-- Whatever your host has already worked out about who is asking. -/
structure Caller where
  accountId : String

def weather : ToolDef IO Caller where
  tool := {
    name := "get_weather"
    description := some "Current conditions for a city"
    inputSchema := Json.mkObj
      [("type", Json.str "object"),
       ("properties", Json.mkObj [("city", Json.mkObj [("type", Json.str "string")])]),
       ("required", Json.arr #[Json.str "city"]),
       ("additionalProperties", Json.bool false)]
    annotations := { title := some "Weather", readOnlyHint := some true }
  }
  run := fun _caller arguments => do
    match arguments.get? ["city"] with
    | some (.str city) => return ToolResult.text s!"It is raining in {city}."
    | _ => return ToolResult.failure "city is required, and must be a string"

def server : Server IO Caller where
  info := { name := "weather", version := "0.1.0" }
  instructions := some "Ask for the weather in a city."
  tools := #[weather]

def config : Config := { allowOrigin := fun origin => origin == "https://app.example.com" }

def handle (caller : Caller) (request : HttpRequest) : IO HttpResponse :=
  serve config server caller request
```

`m` needs `Monad` and nothing else, so tools that do no I/O can run in `Id` and be tested without one.

## Who is asking

Resolve a caller from the request before you dispatch, and pass it in. If you cannot resolve one, answer with the challenge instead of dispatching:

```lean
def metadata : ProtectedResourceMetadata where
  resource := "https://weather.example.com/mcp"
  authorizationServers := #["https://auth.example.com"]
  scopesSupported := #["weather:read"]

def challenge : String :=
  (AuthFailure.unauthenticated #["weather:read"]).challenge
    "https://weather.example.com/.well-known/oauth-protected-resource"
```

Serve `metadata.toJson` from `/.well-known/oauth-protected-resource`, and send `challenge` as the `WWW-Authenticate` header of the refusal. `AuthFailure.status` gives the status that goes with it: `401` unauthenticated, `403` for a scope failure, `400` for a malformed credential. These are OAuth artefacts rather than MCP ones, and they sit here only because MCP makes them mandatory and they are small.

`bearer? (headers.getLast? "authorization")` reads the credential out of the request in the only form MCP allows. It matches the scheme case-insensitively, and anything else, including a header in another scheme, reads as no credential at all.

## MCP Specification Revisions

This library supports [`2026-07-28`](https://modelcontextprotocol.io/specification/2026-07-28), [`2025-11-25`](https://modelcontextprotocol.io/specification/2025-11-25), [`2025-06-18`](https://modelcontextprotocol.io/specification/2025-06-18) and [`2025-03-26`](https://modelcontextprotocol.io/specification/2025-03-26).

**No `Mcp-Session-Id` is ever issued.** Every request stands alone. That is what lets this sit behind a function URL that freezes between invocations, and it means a tool needing state from one call to the next must hand back an explicit handle and take it as an argument, rather than expect the server to remember.

## The transport boundary

An example using [`lean-routing`](https://github.com/paulbutcher/lean-routing):

```lean
import MCP
import Routing
import Std.Http.Server

open Std Http Server
open Routing

route_table Weather
  [ mcp := "/mcp",
    resourceMetadata := "/.well-known/oauth-protected-resource" ]

def endpoint : Result := fun request => do
  let bytes ← MCP.StdHttp.collect request.body
  let caller ← resolveCaller request.line.headers
  let response ← MCP.serve config server caller (MCP.StdHttp.requestOf request.line bytes)
  MCP.StdHttp.responseOf response

def metadataEndpoint : Result := fun _request =>
  Response.ok.json metadata.toJson.compress

def app : StatelessHandler :=
  [ .post Weather.patterns.mcp endpoint,
    .get Weather.patterns.mcp endpoint,
    .delete Weather.patterns.mcp endpoint,
    .get Weather.patterns.resourceMetadata metadataEndpoint
  ] |> toHandler
```

Then hand `app` to `Std.Http.Server` as you would any other `StatelessHandler`.

## Building

`lake build` builds the library; `lake test` runs the tests.
