/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Std.Http
public import MCP.Http

public section

/-!
The layer between `Std.Http` and `MCP.serve`.

It converts, and does nothing else. Reading the body and resolving the caller stay with the
host, because both are its business and neither is the protocol's.
-/

namespace MCP.StdHttp

open Std.Http
open Std Async

/-- A header this module names as a literal, which is why no validation is needed here. -/
def header? (headers : Headers) (name : Header.Name) : Option String :=
  (headers.getLast? name).map toString

def originHeader : Header.Name := .mk "origin"
def protocolVersionHeader : Header.Name := .mk "mcp-protocol-version"
def methodHeader : Header.Name := .mk "mcp-method"
def nameHeader : Header.Name := .mk "mcp-name"

/-- The request line and the body, as the parts of a request the protocol reads. -/
def requestOf (head : Request.Head) (body : ByteArray) : HttpRequest where
  method := toString head.method
  origin := header? head.headers originHeader
  protocolVersion := header? head.headers protocolVersionHeader
  mcpMethod := header? head.headers methodHeader
  mcpName := header? head.headers nameHeader
  body := body

/-- The request body, read to the end. -/
def collect (body : Body.Stream) : Async ByteArray := do
  let mut out := ByteArray.empty
  for chunk in body do
    out := out ++ chunk.data
  return out

def statusOf (code : Nat) : Status :=
  (Status.ofCode none (UInt16.ofNat code)).getD .internalServerError

/-- A header this library did not choose the name of is dropped rather than trusted. -/
def headersOf (hs : Array (String × String)) : Headers :=
  hs.foldl (fun acc (k, v) => (acc.insert? k v).getD acc) Headers.empty

def responseOf (r : HttpResponse) : Async (Std.Http.Response Body.Any) := do
  let body ← Body.Full.ofString r.body
  let head := Std.Http.Response.Builder.new.status (statusOf r.status)
  return (head.headers (headersOf r.headers)).body (Body.Any.ofBody body)

end MCP.StdHttp
