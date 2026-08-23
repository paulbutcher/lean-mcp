/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Json

public section

@[expose] section

namespace MCP

/--
The document RFC 9728 defines and MCP requires of every protected server, served from
`/.well-known/oauth-protected-resource`.

Only the members MCP's flow reads are modelled. The rest of RFC 9728 is optional, and an
application that wants one can add it to the object this produces.
-/
structure ProtectedResourceMetadata where
  /-- The canonical URI of this server, which is also the `resource` of a token request. -/
  resource : String
  authorizationServers : Array String := #[]
  scopesSupported : Array String := #[]
  bearerMethodsSupported : Array String := #["header"]
deriving DecidableEq, Repr, Inhabited

def ProtectedResourceMetadata.toJson (m : ProtectedResourceMetadata) : Json :=
  let strings (a : Array String) : Json := Json.arr (a.map Json.str)
  let optional (k : String) (a : Array String) : List (String × Json) :=
    if a.isEmpty then [] else [(k, strings a)]
  Json.mkObj
    ([("resource", Json.str m.resource)]
      ++ optional "authorization_servers" m.authorizationServers
      ++ optional "scopes_supported" m.scopesSupported
      ++ optional "bearer_methods_supported" m.bearerMethodsSupported)

/-- Why a request was not authorized, which is the whole of what this library knows about it. -/
inductive AuthFailure where
  /-- No usable credential. The challenge names the scopes that would suffice, if any. -/
  | unauthenticated (scope : Array String)
  /-- A valid credential that does not carry enough. `scope` is what this operation needs. -/
  | insufficientScope (scope : Array String) (description : Option String)
  /-- The `Authorization` header itself was malformed. -/
  | malformed (description : Option String)
deriving DecidableEq, Repr, Inhabited

def AuthFailure.status : AuthFailure → Nat
  | .unauthenticated _ => 401
  | .insufficientScope _ _ => 403
  | .malformed _ => 400

/-- A quoted-string, with the two characters RFC 9110 requires escaping escaped. -/
def quoted (s : String) : String :=
  let escape (acc : String) (c : Char) : String :=
    if c == '"' || c == '\\' then (acc.push '\\').push c else acc.push c
  "\"" ++ s.foldl escape "" ++ "\""

/-- The members of `a` separated by single spaces, as a `scope` parameter carries them. -/
def spaceJoined (a : Array String) : String := String.intercalate " " a.toList

/--
The `WWW-Authenticate` value to send with `failure`.

`resourceMetadata` is the URL of the document above; a client reads it from here to find the
authorization server, so a challenge without it leaves the client nowhere to go.
-/
def AuthFailure.challenge (failure : AuthFailure) (resourceMetadata : String) : String :=
  let scopeParam (a : Array String) : List String :=
    if a.isEmpty then [] else [s!"scope={quoted (spaceJoined a)}"]
  let describe (d : Option String) : List String :=
    match d with
    | none => []
    | some d => [s!"error_description={quoted d}"]
  let metadata := s!"resource_metadata={quoted resourceMetadata}"
  let params := match failure with
    | .unauthenticated scope => metadata :: scopeParam scope
    | .insufficientScope scope description =>
      ["error=\"insufficient_scope\""] ++ scopeParam scope ++ [metadata] ++ describe description
    | .malformed description => ["error=\"invalid_request\"", metadata] ++ describe description
  "Bearer " ++ String.intercalate ", " params

end MCP

end
