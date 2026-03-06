import Bliku.Tui.Window
import Bliku.Tui.Model

namespace Bliku.Tui

structure SelectionState where
  anchor : Cursor
  cursor : Cursor
  block : Bool := false
  deriving Repr, Inhabited

structure CommandLineState where
  leader : String
  text : String
  cursorCol : Nat := 0
  deriving Repr, Inhabited

structure OverlayView where
  title : String
  lines : Array String
  maxWidth : Nat := 0
  cursorRow : Nat := 0
  cursorCol : Nat := 0
  deriving Repr, Inhabited

structure CompletionView where
  items : Array CompletionItem
  selected : Nat := 0
  anchorRow : Nat
  anchorCol : Nat
  deriving Repr, Inhabited

structure WindowChrome where
  statusLine : String := ""
  deriving Repr, Inhabited

structure RenderInput where
  selection : Option SelectionState := none
  commandLine : Option CommandLineState := none
  messageLine : String := ""
  statusLine : String := ""
  overlay : Option OverlayView := none
  completion : Option CompletionView := none
  hideTerminalCursor : Bool := false
  deriving Inhabited

end Bliku.Tui
