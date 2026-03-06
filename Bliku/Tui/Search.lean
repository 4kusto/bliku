namespace Bliku.Tui

/-- Find all occurrences of a byte pattern within a byte array. -/
def findAllMatchesBytes (haystack : ByteArray) (needle : ByteArray) : Array (Nat × Nat) := Id.run do
  let n := needle.size
  let h := haystack.size
  if n == 0 || h < n then
    return #[]

  let limit := h - n + 1
  let mut out : Array (Nat × Nat) := #[]
  for i in [0:limit] do
    let mut matched := true
    for j in [0:n] do
      if matched && haystack[i + j]! != needle[j]! then
        matched := false
    if matched then
      out := out.push (i, i + n)
  return out

def overlapsByteRange (r : Nat × Nat) (byteStart byteEnd : Nat) : Bool :=
  let (s, e) := r
  byteStart < e && byteEnd > s

def activeMatchRange (hitRanges : Array (Nat × Nat)) (cursorByte : Option Nat) : Option (Nat × Nat) :=
  match cursorByte with
  | none => none
  | some cb =>
      let rec loop (i : Nat) : Option (Nat × Nat) :=
        if i >= hitRanges.size then
          none
        else
          let m := hitRanges[i]!
          let (s, e) := m
          if s <= cb && cb < e then
            some m
          else
            loop (i + 1)
      loop 0

end Bliku.Tui
