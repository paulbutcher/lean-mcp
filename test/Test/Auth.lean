/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Test.Runner

/-!
The OAuth artefacts MCP makes mandatory, and the header decoding the transport makes mandatory.
-/

namespace MCP.Test

def metadata : ProtectedResourceMetadata where
  resource := "https://mcp.example.com/mcp"
  authorizationServers := #["https://auth.example.com"]
  scopesSupported := #["files:read"]

def sparseMetadata : ProtectedResourceMetadata := { resource := "https://mcp.example.com/mcp" }

def resourceMetadataUrl : String :=
  "https://mcp.example.com/.well-known/oauth-protected-resource"

def testAuth (r : Results) : Results :=
  let document := metadata.toJson
  let sparse := { sparseMetadata with bearerMethodsSupported := #[] }.toJson
  let unauthenticated := (AuthFailure.unauthenticated #["files:read", "files:write"]).challenge
    resourceMetadataUrl
  let insufficient :=
    (AuthFailure.insufficientScope #["files:write"] (some "Write access is required")).challenge
      resourceMetadataUrl
  let malformed := (AuthFailure.malformed none).challenge resourceMetadataUrl
  r
  |>.check "metadata names the resource"
    (document.get? ["resource"] == some (Json.str "https://mcp.example.com/mcp"))
  |>.check "metadata names the authorization server"
    (document.get? ["authorization_servers", .index 0]
      == some (Json.str "https://auth.example.com"))
  |>.check "metadata defaults to the header method"
    (document.get? ["bearer_methods_supported", .index 0] == some (Json.str "header"))
  |>.check "metadata omits what it has nothing to say about"
    (lacksField sparse ["authorization_servers"]
      && lacksField sparse ["scopes_supported"]
      && lacksField sparse ["bearer_methods_supported"])
  |>.check "an unauthenticated request is a 401"
    ((AuthFailure.unauthenticated #[]).status == 401)
  |>.check "a scope failure is a 403"
    ((AuthFailure.insufficientScope #[] none).status == 403)
  |>.check "a malformed credential is a 400" ((AuthFailure.malformed none).status == 400)
  |>.check "a challenge points at the metadata document"
    (containsText unauthenticated s!"resource_metadata=\"{resourceMetadataUrl}\"")
  |>.check "a challenge joins its scopes with spaces"
    (containsText unauthenticated "scope=\"files:read files:write\"")
  |>.check "a scope failure names the reason"
    (containsText insufficient "error=\"insufficient_scope\""
      && containsText insufficient "scope=\"files:write\""
      && containsText insufficient "error_description=\"Write access is required\"")
  |>.check "a malformed credential names the reason"
    (containsText malformed "error=\"invalid_request\"")
  |>.check "a challenge escapes what would end the quoted string early"
    ((AuthFailure.unauthenticated #[]).challenge "a\"b\\c"
      == "Bearer resource_metadata=\"a\\\"b\\\\c\"")

/-- The spec's own example of a value the header grammar cannot carry as it stands. -/
def testHeaderDecoding (r : Results) : Results :=
  r
  |>.check "an encoded header value is decoded"
    (Base64.decodeHeaderValue? "=?base64?SGVsbG8sIOS4lueVjA==?=" == some "Hello, 世界")
  |>.check "a plain value is already itself"
    (Base64.decodeHeaderValue? "get_weather" == some "get_weather")
  |>.check "a value that is not valid Base64 is refused"
    (Base64.decodeHeaderValue? "=?base64?not base64!?=").isNone

end MCP.Test
