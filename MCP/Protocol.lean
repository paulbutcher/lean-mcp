/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public section

@[expose] section

namespace MCP

/--
A revision of the Model Context Protocol that this server speaks.

The set is closed, which is what makes "the version a negotiation reply names is one the server
supports" a fact about the type rather than a property to be checked.
-/
inductive Version where
  | v2026_07_28
  | v2025_11_25
  | v2025_06_18
  | v2025_03_26
deriving DecidableEq, Repr, Inhabited

/--
How a revision establishes the version in force.

`modern` carries it on every request; `legacy` agrees it once in an `initialize` handshake.
-/
inductive Era where
  | modern
  | legacy
deriving DecidableEq, Repr, Inhabited

namespace Version

/-- Every supported revision, newest first. -/
def all : List Version := [.v2026_07_28, .v2025_11_25, .v2025_06_18, .v2025_03_26]

/-- The newest supported revision, and the one an out-of-range `initialize` is answered with. -/
def latest : Version := .v2026_07_28

def toString : Version → String
  | .v2026_07_28 => "2026-07-28"
  | .v2025_11_25 => "2025-11-25"
  | .v2025_06_18 => "2025-06-18"
  | .v2025_03_26 => "2025-03-26"

instance : ToString Version := ⟨toString⟩

def ofString? (s : String) : Option Version :=
  match s with
  | "2026-07-28" => some .v2026_07_28
  | "2025-11-25" => some .v2025_11_25
  | "2025-06-18" => some .v2025_06_18
  | "2025-03-26" => some .v2025_03_26
  | _ => none

def era : Version → Era
  | .v2026_07_28 => .modern
  | .v2025_11_25 => .legacy
  | .v2025_06_18 => .legacy
  | .v2025_03_26 => .legacy

/--
What to assume when a request carries no `MCP-Protocol-Version` header.

The transport permits this only of a server that serves clients older than `2025-06-18`, which
did not define the header. This one does, so it takes the allowance.
-/
def headerDefault : Version := .v2025_03_26

/--
The version an `initialize` naming `s` is answered with: `s` itself when it is supported, and
otherwise the newest that is, as the legacy lifecycle requires of a server that cannot meet the
request.
-/
def negotiate (s : String) : Version := (ofString? s).getD latest

/-- The supported revisions as the `data.supported` of an `UnsupportedProtocolVersionError`. -/
def allStrings : List String := all.map toString

end Version

end MCP

end
