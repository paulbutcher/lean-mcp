/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Test.Runner

/-!
The adapter between `Std.Http` and the protocol core.

That every header this library emits is one `Std.Http` accepts would be better as a theorem, but
the kernel will not reduce `Header.Name.ofString?` past the string operations it validates with,
so these run the check instead.
-/

namespace MCP.Test

def testTransport (r : Results) : Results :=
  r
  |>.check "the headers of a JSON response can be built"
    (MCP.StdHttp.headersOf (jsonResponse 200 (Json.mkObj [])).headers).isSome
  |>.check "the headers of a 405 can be built"
    (MCP.StdHttp.headersOf methodNotAllowed.headers).isSome
  |>.check "a header that cannot be built is not passed off as one that was never asked for"
    (MCP.StdHttp.headersOf #[("bad name", "value")]).isNone

end MCP.Test
