/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import MCP

namespace MCP.Test

/-- Collects every failing check by name rather than stopping at the first, so one run reports
the whole set of regressions. -/
structure Results where
  failures : Array String := #[]

def Results.check (r : Results) (name : String) (cond : Bool) : Results :=
  if cond then r else { r with failures := r.failures.push name }

def Results.report (r : Results) : IO UInt32 := do
  if r.failures.isEmpty then
    IO.println "All tests passed."
    return 0
  else
    IO.eprintln s!"{r.failures.size} test(s) failed:"
    for name in r.failures do
      IO.eprintln s!"  - {name}"
    return 1

def objectSchema : Json :=
  Json.mkObj [("type", Json.str "object"), ("additionalProperties", Json.bool false)]

/-- Repeats what it was given, so a test can see the arguments the dispatcher passed on. -/
def echoTool : ToolDef Id Unit where
  tool := {
    name := "echo"
    inputSchema := objectSchema
    description := some "Repeats its arguments"
    annotations := { title := some "Echo", readOnlyHint := some true }
  }
  run := fun _ args => ToolResult.text (toString args)

/-- Fails the way a tool is meant to fail: a result the model can read, not a protocol error. -/
def refusingTool : ToolDef Id Unit where
  tool := { name := "refuse", inputSchema := objectSchema }
  run := fun _ _ => ToolResult.failure "the tool refused"

def server : Server Id Unit where
  info := { name := "test-server", version := "0.1.0" }
  instructions := some "Two tools, one of which always refuses."
  tools := #[echoTool, refusingTool]

def config : Config := { allowOrigin := fun o => o == "https://allowed.example" }

/-- One POST to the endpoint, with only the headers a test cares to set. -/
def post (body : String) (protocolVersion mcpMethod mcpName origin : Option String := none) :
    HttpResponse :=
  serve config server ()
    { method := "POST", body := body.toUTF8, protocolVersion, mcpMethod, mcpName, origin }

def bodyJson (r : HttpResponse) : Json := (Json.parse r.body).toOption.getD Json.null

/-- The value at `path` in a response body, for asserting about one field at a time. -/
def at? (r : HttpResponse) (path : Json.Path) : Option Json := (bodyJson r).get? path

/-- Whether a response body has anything at `path`. -/
def hasPath (r : HttpResponse) (path : Json.Path) : Bool := (at? r path).isSome

def lacksPath (r : HttpResponse) (path : Json.Path) : Bool := (at? r path).isNone

def lacksField (j : Json) (path : Json.Path) : Bool := (j.get? path).isNone

/-- Whether `needle` occurs in `haystack`, for asserting about part of a built string. -/
def containsText (haystack needle : String) : Bool := (haystack.splitOn needle).length ≥ 2

def hasHeader (r : HttpResponse) (name : String) : Bool :=
  r.headers.any (fun (k, _) => k.toLower == name.toLower)

def headerValue? (r : HttpResponse) (name : String) : Option String :=
  (r.headers.find? (fun (k, _) => k.toLower == name.toLower)).map Prod.snd

end MCP.Test
