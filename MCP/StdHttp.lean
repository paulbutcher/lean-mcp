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

/--
The response headers, or nothing if one of them cannot be built.

Every name and value here is one this library chose, so a failure is this library being wrong
rather than anything the request asked for. Dropping the offender would send a `200` that lies
about its own content type, so `responseOf` sends a `500` instead.
-/
def headersOf (hs : Array (String × String)) : Option Headers :=
  hs.foldlM (fun acc (k, v) => acc.insert? k v) Headers.empty

def responseOf (r : HttpResponse) : Async (Std.Http.Response Body.Any) := do
  let (status, headers, text) := match headersOf r.headers with
    | some headers => (statusOf r.status, headers, r.body)
    | none => (Status.internalServerError, Headers.empty, "")
  let body ← Body.Full.ofString text
  let head := Std.Http.Response.Builder.new.status status
  return (head.headers headers).body (Body.Any.ofBody body)

end MCP.StdHttp
