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

/-!
Error codes this server emits.

`-32020` upwards is the sub-range the specification reserves to itself; a code in it may only be
one the specification defines, used as the specification defines it. Nothing here invents one.
-/
namespace ErrorCode

def parseError : Int := -32700
def invalidRequest : Int := -32600
def methodNotFound : Int := -32601
def invalidParams : Int := -32602
def internalError : Int := -32603

/-- The headers do not agree with the body, or a required one is missing or malformed. -/
def headerMismatch : Int := -32020

/-- The requested protocol version is one this server does not implement. -/
def unsupportedProtocolVersion : Int := -32022

end ErrorCode

/-- A JSON-RPC identifier. The protocol forbids `null` here, so no case represents it. -/
inductive RequestId where
  | str (s : String)
  | num (n : Int)
deriving DecidableEq, Repr, Inhabited

def RequestId.toJson : RequestId → Json
  | .str s => Json.str s
  | .num n => Json.ofInt n

def RequestId.fromJson? : Json → Except String RequestId
  | .str s => .ok (.str s)
  | j@(.num _) => (RequestId.num ·) <$> Json.getInt? j
  | _ => .error "request id must be a string or an integer"

structure ErrorObject where
  code : Int
  message : String
  data : Option Json := none
deriving DecidableEq, Repr, Inhabited

def ErrorObject.toJson (e : ErrorObject) : Json :=
  Json.mkObj
    ([("code", Json.ofInt e.code), ("message", Json.str e.message)] ++ Json.opt "data" e.data)

/--
What a request is answered with.

The two cases are disjoint by construction, which is the point: a tool that failed is a
`result` whose `ToolResult` has `isError` set, and only a request that could not be dispatched
at all becomes an `error`. No value of this type is both, and none is neither.

The identifier is optional on `error` alone, for the one case the specification allows it: a
request whose id could not be read because the request could not be parsed.
-/
inductive Response where
  | result (id : RequestId) (value : Json)
  | error (id : Option RequestId) (err : ErrorObject)
deriving DecidableEq, Repr, Inhabited

def Response.id? : Response → Option RequestId
  | .result id _ => some id
  | .error id _ => id

def Response.toJson : Response → Json
  | .result id v =>
    Json.mkObj [("jsonrpc", Json.str "2.0"), ("id", id.toJson), ("result", v)]
  | .error id e =>
    Json.mkObj
      [("jsonrpc", Json.str "2.0"),
       ("id", match id with | some i => i.toJson | none => Json.null),
       ("error", e.toJson)]

/-- A parsed JSON-RPC message. `id` is absent exactly when the message is a notification. -/
structure Request where
  id : Option RequestId := none
  method : String
  params : Json := Json.null
deriving DecidableEq, Repr, Inhabited

/--
Reads the JSON-RPC envelope. The `jsonrpc` member is not enforced: rejecting a message that is
otherwise well formed and unambiguous buys nothing, and several clients in the field omit it.
-/
def Request.fromJson? (j : Json) : Except String Request := do
  let method ← reqField? j "method" String
  let id ← match j with
    | .obj fields =>
      match Json.findLast? fields "id" with
      | none => .ok none
      | some Json.null => .ok none
      | some v => (Option.some ·) <$> RequestId.fromJson? v
    | _ => .error "expected an object"
  let params ← optField? j "params" Json
  return { id, method, params := params.getD Json.null }

/-- A JSON-RPC error together with the HTTP status the transport must carry it on. -/
structure Failure where
  status : Nat
  err : ErrorObject
deriving DecidableEq, Repr, Inhabited

/-- A response and the status it travels on. -/
structure Reply where
  status : Nat
  body : Response
deriving DecidableEq, Repr, Inhabited

/-- The one place a dispatched request becomes a response, and so the one place an id is fixed. -/
def Reply.ofOutcome (id : RequestId) : Except Failure Json → Reply
  | .ok v => { status := 200, body := .result id v }
  | .error f => { status := f.status, body := .error (some id) f.err }

/-- The answer to a request whose id could not be read, because the request could not be read. -/
def Reply.unreadable (code : Int) (message : String) : Reply :=
  { status := 400, body := .error none { code, message } }

end MCP

end
