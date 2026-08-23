/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import MCP

/-! The shape the README documents, compiled here so that it cannot drift from the library. -/

namespace MCP.Readme

/-- Whatever the host has already worked out about who is asking. -/
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

def metadata : ProtectedResourceMetadata where
  resource := "https://weather.example.com/mcp"
  authorizationServers := #["https://auth.example.com"]
  scopesSupported := #["weather:read"]

def challenge : String :=
  (AuthFailure.unauthenticated #["weather:read"]).challenge
    "https://weather.example.com/.well-known/oauth-protected-resource"

end MCP.Readme
