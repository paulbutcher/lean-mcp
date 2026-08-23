/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Test.Runner

/-!
A whole conversation with each era, as the client of that era would hold it.
-/

namespace MCP.Test

def rpc (id : Nat) (method : String) (params : Json) : String :=
  (Json.mkObj
    [("jsonrpc", Json.str "2.0"), ("id", Json.ofNat id), ("method", Json.str method),
     ("params", params)]).compress

def notification (method : String) : String :=
  (Json.mkObj [("jsonrpc", Json.str "2.0"), ("method", Json.str method)]).compress

def modernMeta : Json :=
  Json.mkObj
    [(protocolVersionKey, Json.str "2026-07-28"), (clientCapabilitiesKey, Json.mkObj [])]

def modernParams (fields : List (String × Json)) : Json :=
  Json.mkObj (fields ++ [("_meta", modernMeta)])

def modernDiscover : HttpResponse :=
  post (rpc 1 "server/discover" (modernParams []))
    (protocolVersion := some "2026-07-28") (mcpMethod := some "server/discover")

def modernList : HttpResponse :=
  post (rpc 2 "tools/list" (modernParams []))
    (protocolVersion := some "2026-07-28") (mcpMethod := some "tools/list")

def modernCall : HttpResponse :=
  post
    (rpc 3 "tools/call"
      (modernParams
        [("name", Json.str "echo"), ("arguments", Json.mkObj [("word", Json.str "hello")])]))
    (protocolVersion := some "2026-07-28") (mcpMethod := some "tools/call")
    (mcpName := some "echo")

def testModern (r : Results) : Results :=
  r
  |>.check "modern discover status" (modernDiscover.status == 200)
  |>.check "modern discover resultType"
    (at? modernDiscover ["result", "resultType"] == some (Json.str "complete"))
  |>.check "modern discover names this revision first"
    (at? modernDiscover ["result", "supportedVersions", .index 0] == some (Json.str "2026-07-28"))
  |>.check "modern discover offers tools"
    (at? modernDiscover ["result", "capabilities", "tools"] == some (Json.mkObj []))
  |>.check "modern discover identifies the server"
    (at? modernDiscover ["result", "_meta", "io.modelcontextprotocol/serverInfo", "name"]
      == some (Json.str "test-server"))
  |>.check "modern discover carries a cache scope"
    (at? modernDiscover ["result", "cacheScope"] == some (Json.str "private"))
  |>.check "modern discover carries a ttl" (hasPath modernDiscover ["result", "ttlMs"])
  |>.check "modern list status" (modernList.status == 200)
  |>.check "modern list is in configured order"
    (at? modernList ["result", "tools", .index 0, "name"] == some (Json.str "echo")
      && at? modernList ["result", "tools", .index 1, "name"] == some (Json.str "refuse"))
  |>.check "modern list carries the annotations"
    (at? modernList ["result", "tools", .index 0, "annotations", "title"] == some (Json.str "Echo")
      && at? modernList ["result", "tools", .index 0, "annotations", "readOnlyHint"]
        == some (Json.bool true))
  |>.check "modern list carries the caching fields the revision requires"
    (hasPath modernList ["result", "ttlMs"]
      && hasPath modernList ["result", "cacheScope"])
  |>.check "modern call status" (modernCall.status == 200)
  |>.check "modern call succeeded"
    (at? modernCall ["result", "isError"] == some (Json.bool false))
  |>.check "modern call returned text"
    (at? modernCall ["result", "content", .index 0, "type"] == some (Json.str "text"))
  |>.check "modern call saw its arguments"
    ((at? modernCall ["result", "content", .index 0, "text"]).any
      (fun j => match j with | .str s => containsText s "hello" | _ => false))
  |>.check "modern call answers under the request's id"
    (at? modernCall ["id"] == some (Json.ofNat 3))

def legacyInitialize : HttpResponse :=
  post
    (rpc 1 "initialize"
      (Json.mkObj
        [("protocolVersion", Json.str "2025-11-25"),
         ("capabilities", Json.mkObj []),
         ("clientInfo", Json.mkObj
           [("name", Json.str "legacy-client"), ("version", Json.str "1.0.0")])]))

def legacyInitialized : HttpResponse := post (notification "notifications/initialized")

def legacyList : HttpResponse :=
  post (rpc 2 "tools/list" (Json.mkObj [])) (protocolVersion := some "2025-11-25")

/-- The legacy revisions make `params` optional, and a client of one of them may omit it. -/
def legacyListNoParams : HttpResponse :=
  serve config server ()
    { method := "POST"
      protocolVersion := some "2025-11-25"
      body := (Json.mkObj
        [("jsonrpc", Json.str "2.0"), ("id", Json.ofNat 2),
         ("method", Json.str "tools/list")]).compress.toUTF8 }

def legacyCall : HttpResponse :=
  post
    (rpc 3 "tools/call"
      (Json.mkObj
        [("name", Json.str "echo"), ("arguments", Json.mkObj [("word", Json.str "hello")])]))
    (protocolVersion := some "2025-11-25")

def testLegacy (r : Results) : Results :=
  r
  |>.check "legacy initialize status" (legacyInitialize.status == 200)
  |>.check "legacy initialize agrees the version the client asked for"
    (at? legacyInitialize ["result", "protocolVersion"] == some (Json.str "2025-11-25"))
  |>.check "legacy initialize identifies the server"
    (at? legacyInitialize ["result", "serverInfo", "name"] == some (Json.str "test-server"))
  |>.check "legacy initialize offers tools"
    (at? legacyInitialize ["result", "capabilities", "tools"] == some (Json.mkObj []))
  |>.check "legacy initialize passes on the instructions"
    (hasPath legacyInitialize ["result", "instructions"])
  |>.check "legacy initialize issues no session"
    (!hasHeader legacyInitialize "Mcp-Session-Id")
  |>.check "legacy initialize carries no modern result fields"
    (lacksPath legacyInitialize ["result", "resultType"])
  |>.check "legacy initialized notification is accepted"
    (legacyInitialized.status == 202 && legacyInitialized.body == "")
  |>.check "legacy list status" (legacyList.status == 200)
  |>.check "legacy list is in configured order"
    (at? legacyList ["result", "tools", .index 0, "name"] == some (Json.str "echo")
      && at? legacyList ["result", "tools", .index 1, "name"] == some (Json.str "refuse"))
  |>.check "legacy list carries no field its revision does not define"
    (lacksPath legacyList ["result", "resultType"]
      && lacksPath legacyList ["result", "ttlMs"]
      && lacksPath legacyList ["result", "_meta"])
  |>.check "legacy call status" (legacyCall.status == 200)
  |>.check "legacy call succeeded"
    (at? legacyCall ["result", "isError"] == some (Json.bool false))
  |>.check "a legacy list with no params at all is still answered"
    (legacyListNoParams.status == 200
      && at? legacyListNoParams ["result", "tools", .index 0, "name"] == some (Json.str "echo"))
  |>.check "legacy exchange issues no session at any point"
    (!hasHeader legacyList "Mcp-Session-Id" && !hasHeader legacyCall "Mcp-Session-Id")

/-- The one `Tool` encoding is the one both eras read, so the same bytes appear in both lists. -/
def testOneEncoding (r : Results) : Results :=
  r.check "both eras list the same tools"
    (at? modernList ["result", "tools"] == at? legacyList ["result", "tools"])

end MCP.Test
