namespace Bliku.Tui.Terminal

/-- Get terminal size (rows, cols). -/
def getWindowSize : IO (Nat × Nat) := do
  let out ← IO.Process.run { cmd := "stty", args := #["-F", "/dev/tty", "size"] }
  let parts := out.trimAscii.toString.splitOn " "
  match parts with
  | [rows, cols] => pure (rows.toNat!, cols.toNat!)
  | _ => pure (24, 80)

def moveCursorStr (row col : Nat) : String :=
  s!"\x1b[{row + 1};{col + 1}H"

def hideCursorStr : String := "\x1b[?25l"
def showCursorStr : String := "\x1b[?25h"
def clearLineStr : String := "\x1b[K"
def homeCursorStr : String := "\x1b[H"
def clearScreenStr : String := "\x1b[2J"

end Bliku.Tui.Terminal
