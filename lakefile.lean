/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lake
open Lake DSL

package MCP where
  version := v!"0.3.0"
  leanOptions := #[⟨`warningAsError, true⟩]

require json from git "https://github.com/paulbutcher/lean-json"@"v0.3.1"

@[default_target]
lean_lib MCP

-- Tests live in their own package so that test-only dependencies stay out of the
-- dependency graph a consumer resolves.
@[test_driver]
script tests do
  let child ← IO.Process.spawn
    { cmd := "lake", args := #["test"], cwd := __dir__ / "test" }
  child.wait
