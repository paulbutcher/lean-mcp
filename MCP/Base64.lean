/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public section

@[expose] section

namespace MCP.Base64

def value? (c : Char) : Option Nat :=
  if 'A' ≤ c && c ≤ 'Z' then some (c.toNat - 'A'.toNat)
  else if 'a' ≤ c && c ≤ 'z' then some (c.toNat - 'a'.toNat + 26)
  else if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat + 52)
  else if c == '+' then some 62
  else if c == '/' then some 63
  else none

/--
`acc` holds the `bits` low-order bits not yet emitted. Trailing bits shorter than a byte are
discarded, as they are the padding of the final group.
-/
def decodeLoop : List Char → Nat → Nat → Array UInt8 → Option (Array UInt8)
  | [], _, _, out => some out
  | '=' :: rest, _, _, out => if rest.all (· == '=') then some out else none
  | c :: rest, acc, bits, out =>
    match value? c with
    | none => none
    | some v =>
      let acc := acc * 64 + v
      let bits := bits + 6
      if bits ≥ 8 then
        let bits := bits - 8
        decodeLoop rest (acc % 2 ^ bits) bits (out.push (UInt8.ofNat (acc / 2 ^ bits)))
      else
        decodeLoop rest acc bits out

def decode? (s : String) : Option ByteArray :=
  (⟨·⟩) <$> decodeLoop s.toList 0 0 #[]

/-- The prefix and suffix that mark a header value as Base64-encoded UTF-8. -/
def sentinelPrefix : String := "=?base64?"
def sentinelSuffix : String := "?="

/--
A mirrored header value as the body value it must equal.

The transport carries a value the header grammar cannot hold inside `=?base64?...?=`, and
requires that a server decode it before comparing. A value not in that form is already itself.
-/
def decodeHeaderValue? (s : String) : Option String :=
  if s.startsWith sentinelPrefix && s.endsWith sentinelSuffix
      && s.length ≥ sentinelPrefix.length + sentinelSuffix.length then
    let inner := ((s.drop sentinelPrefix.length).dropEnd sentinelSuffix.length).copy
    decode? inner >>= String.fromUTF8?
  else
    some s

end MCP.Base64

end
