/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Test.Exchange

/-!
The ways a request can be refused, and the status and code each one must be refused with.
-/

namespace MCP.Test

def errorCode? (r : HttpResponse) : Option Json := at? r ["error", "code"]

def unsupportedVersion : HttpResponse :=
  post (rpc 1 "tools/list" (Json.mkObj [])) (protocolVersion := some "1999-01-01")

def missingVersionHeader : HttpResponse := post (rpc 1 "tools/list" (Json.mkObj []))

def badOrigin : HttpResponse :=
  post (rpc 1 "tools/list" (Json.mkObj [])) (origin := some "https://evil.example")

def goodOrigin : HttpResponse :=
  post (rpc 1 "tools/list" (Json.mkObj [])) (origin := some "https://allowed.example")

def getRequest : HttpResponse :=
  serve config server () { method := "GET" }

def deleteRequest : HttpResponse :=
  serve config server () { method := "DELETE" }

def unknownTool : HttpResponse :=
  post (rpc 4 "tools/call" (Json.mkObj [("name", Json.str "nope")]))
    (protocolVersion := some "2025-11-25")

def refusedCall : HttpResponse :=
  post (rpc 5 "tools/call" (Json.mkObj [("name", Json.str "refuse")]))
    (protocolVersion := some "2025-11-25")

def unparseable : HttpResponse :=
  serve config server () { method := "POST", body := "{ not json".toUTF8 }

def unknownMethodModern : HttpResponse :=
  post (rpc 6 "resources/list" (modernParams []))
    (protocolVersion := some "2026-07-28") (mcpMethod := some "resources/list")

def unknownMethodLegacy : HttpResponse :=
  post (rpc 6 "resources/list" (Json.mkObj [])) (protocolVersion := some "2025-11-25")

def methodHeaderMismatch : HttpResponse :=
  post (rpc 7 "tools/list" (modernParams []))
    (protocolVersion := some "2026-07-28") (mcpMethod := some "tools/call")

def missingMethodHeader : HttpResponse :=
  post (rpc 7 "tools/list" (modernParams [])) (protocolVersion := some "2026-07-28")

def versionHeaderMismatch : HttpResponse :=
  post (rpc 7 "tools/list" (modernParams []))
    (protocolVersion := some "2025-11-25") (mcpMethod := some "tools/list")

def missingClientCapabilities : HttpResponse :=
  post
    (rpc 8 "tools/list"
      (Json.mkObj [("_meta", Json.mkObj [(protocolVersionKey, Json.str "2026-07-28")])]))
    (protocolVersion := some "2026-07-28") (mcpMethod := some "tools/list")

def nameHeaderMismatch : HttpResponse :=
  post (rpc 9 "tools/call" (modernParams [("name", Json.str "echo")]))
    (protocolVersion := some "2026-07-28") (mcpMethod := some "tools/call")
    (mcpName := some "refuse")

/-- `"echo"` in the form the transport requires of a value the header grammar cannot hold. -/
def encodedName : HttpResponse :=
  post (rpc 10 "tools/call" (modernParams [("name", Json.str "echo")]))
    (protocolVersion := some "2026-07-28") (mcpMethod := some "tools/call")
    (mcpName := some "=?base64?ZWNobw==?=")

def initializeUnknownVersion : HttpResponse :=
  post (rpc 1 "initialize" (Json.mkObj [("protocolVersion", Json.str "2024-11-05")]))

def testErrors (r : Results) : Results :=
  r
  |>.check "unsupported version is a 400" (unsupportedVersion.status == 400)
  |>.check "unsupported version uses the code the specification allocates"
    (errorCode? unsupportedVersion == some (Json.ofInt (-32022)))
  |>.check "unsupported version lists what is supported"
    (at? unsupportedVersion ["error", "data", "supported", .index 0]
      == some (Json.str "2026-07-28"))
  |>.check "unsupported version echoes what was asked for"
    (at? unsupportedVersion ["error", "data", "requested"] == some (Json.str "1999-01-01"))
  |>.check "a missing version header reads as the oldest revision"
    (missingVersionHeader.status == 200
      && at? missingVersionHeader ["result", "tools", .index 0, "name"] == some (Json.str "echo")
      && lacksPath missingVersionHeader ["result", "resultType"])
  |>.check "an invalid origin is forbidden" (badOrigin.status == 403)
  |>.check "a forbidden origin is named in the refusal"
    ((at? badOrigin ["error", "message"]).any
      (fun j => match j with | .str m => containsText m "https://evil.example" | _ => false))
  |>.check "an allowed origin is dispatched" (goodOrigin.status == 200)
  |>.check "GET is not allowed"
    (getRequest.status == 405 && headerValue? getRequest "Allow" == some "POST")
  |>.check "DELETE is not allowed" (deleteRequest.status == 405)
  |>.check "an unknown tool is a dispatch failure"
    (unknownTool.status == 200 && errorCode? unknownTool == some (Json.ofInt (-32602)))
  |>.check "a refusing tool is a result, not an error"
    (refusedCall.status == 200
      && at? refusedCall ["result", "isError"] == some (Json.bool true)
      && lacksPath refusedCall ["error"])
  |>.check "an unreadable body is answered under a null id"
    (unparseable.status == 400
      && errorCode? unparseable == some (Json.ofInt (-32700))
      && at? unparseable ["id"] == some Json.null)
  |>.check "the modern transport answers an unknown method with 404"
    (unknownMethodModern.status == 404
      && errorCode? unknownMethodModern == some (Json.ofInt (-32601)))
  |>.check "the legacy transport answers an unknown method on a 200"
    (unknownMethodLegacy.status == 200
      && errorCode? unknownMethodLegacy == some (Json.ofInt (-32601)))
  |>.check "a method header that disagrees with the body is rejected"
    (methodHeaderMismatch.status == 400
      && errorCode? methodHeaderMismatch == some (Json.ofInt (-32020)))
  |>.check "a missing method header is rejected"
    (missingMethodHeader.status == 400
      && errorCode? missingMethodHeader == some (Json.ofInt (-32020)))
  |>.check "a version header that disagrees with the body is rejected"
    (versionHeaderMismatch.status == 400
      && errorCode? versionHeaderMismatch == some (Json.ofInt (-32020)))
  |>.check "a modern request without client capabilities is malformed"
    (missingClientCapabilities.status == 400
      && errorCode? missingClientCapabilities == some (Json.ofInt (-32602)))
  |>.check "a name header that disagrees with the body is rejected"
    (nameHeaderMismatch.status == 400
      && errorCode? nameHeaderMismatch == some (Json.ofInt (-32020)))
  |>.check "an encoded name header is decoded before it is compared"
    (encodedName.status == 200 && at? encodedName ["result", "isError"] == some (Json.bool false))
  |>.check "an initialize naming an unsupported version is answered with the newest supported"
    (initializeUnknownVersion.status == 200
      && at? initializeUnknownVersion ["result", "protocolVersion"]
        == some (Json.str "2026-07-28"))

end MCP.Test
