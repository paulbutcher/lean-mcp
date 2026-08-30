/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Test

open MCP.Test

def main : IO UInt32 :=
  ({} : Results)
    |> testModern
    |> testLegacy
    |> testOneEncoding
    |> testErrors
    |> testAuth
    |> testHeaderDecoding
    |> testTransport
    |>.report
