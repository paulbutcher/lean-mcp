/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Json
public import MCP.Types

public section

@[expose] section

namespace MCP

/--
Who may hold a cached copy of a result.

The wire spellings are `"private"` and `"public"`; the constructors are named for what they
mean, because both wire words are keywords here.
-/
inductive CacheScope where
  /-- `"private"`: the result may depend on who asked, so only that caller may cache it. -/
  | caller
  /-- `"public"`: the result is the same for every caller, and a shared cache may hold it. -/
  | shared
deriving DecidableEq, Repr, Inhabited

def CacheScope.toJson : CacheScope → Json
  | .caller => Json.str "private"
  | .shared => Json.str "public"

/--
One tool: what the protocol says about it, and what to run when it is called.

`run` takes the caller the host resolved and the raw `arguments` object. Validating those
against `tool.inputSchema` is the application's business; this library does not read the schema.
-/
structure ToolDef (m : Type → Type) (α : Type) where
  tool : Tool
  run : α → Json → m ToolResult

/--
Everything the protocol needs to know about an application.

`tools` is an array rather than a map because `tools/list` must be deterministically ordered,
and the order configured here is the order listed.
-/
structure Server (m : Type → Type) (α : Type) where
  info : Implementation
  instructions : Option String := none
  tools : Array (ToolDef m α)

/-- The first tool named `name`, or none. -/
def Server.find? (srv : Server m α) (name : String) : Option (ToolDef m α) :=
  srv.tools.find? (fun t => t.tool.name == name)

/--
Policy the protocol leaves to the deployment.

`allowOrigin` decides which `Origin` headers are acceptable. It rejects everything by default:
an MCP client sends no `Origin` at all, so denying every one that appears is the setting that
costs a correct client nothing and closes DNS rebinding to a browser.

It gates the `Origin` check and nothing else, so allowing an origin is not by itself enough to
let a browser reach the endpoint. `Mcp-Method` and `MCP-Protocol-Version` are not headers CORS
treats as safe, so a browser sends a preflight `OPTIONS` first, which `serve` answers `405`, and
no `Access-Control-*` header appears on any response. A browser client therefore needs the host
to answer preflight and to add those headers itself. CORS is transport policy rather than
protocol, and it sits with the host for the same reason authentication does.

`cacheTtlMs` and `cacheScope` fill the fields the modern revision requires on `tools/list` and
`server/discover`. Zero and `caller` say "do not reuse this", which is right until the
application says otherwise.
-/
structure Config where
  allowOrigin : String → Bool := fun _ => false
  cacheTtlMs : Nat := 0
  cacheScope : CacheScope := .caller

end MCP

end
