namespace Bliku

inductive Color where
  | ansi (code : Nat)
  | rgb (r g b : Nat)
  deriving Repr, BEq, Inhabited

structure Face where
  fg : Option Color := none
  bg : Option Color := none
  bold : Bool := false
  italic : Bool := false
  underline : Bool := false
  deriving Repr, BEq, Inhabited

structure Theme where
  faces : List (String × Face) := []
  deriving Repr, BEq, Inhabited

private def colorToAnsi (channel : String) : Color → String
  | .ansi code => s!"\x1b[{channel};5;{code}m"
  | .rgb r g b => s!"\x1b[{channel};2;{r};{g};{b}m"

def Face.toAnsi (face : Face) : String := Id.run do
  let mut parts : Array String := #[]
  match face.fg with
  | some color => parts := parts.push (colorToAnsi "38" color)
  | none => pure ()
  match face.bg with
  | some color => parts := parts.push (colorToAnsi "48" color)
  | none => pure ()
  if face.bold then
    parts := parts.push "\x1b[1m"
  if face.italic then
    parts := parts.push "\x1b[3m"
  if face.underline then
    parts := parts.push "\x1b[4m"
  return String.intercalate "" parts.toList

end Bliku
