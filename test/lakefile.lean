/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lake
open Lake DSL

package test where
  version := v!"0.1.0"
  leanOptions := #[⟨`warningAsError, true⟩]

require MCP from ".."

@[default_target]
lean_lib Test

@[test_driver]
lean_exe runTests where
  root := `Main
