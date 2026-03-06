import Bliku.Tui.Window
import Bliku.Tui.Syntax.Types

namespace Bliku.Tui

inductive Mode where
  | normal
  | insert
  | command
  | searchForward
  | searchBackward
  | visual
  | visualBlock
  deriving Repr, BEq, Inhabited

instance : ToString Mode where
  toString
    | .normal => "NORMAL"
    | .insert => "INSERT"
    | .command => "COMMAND"
    | .searchForward => "SEARCH"
    | .searchBackward => "SEARCH"
    | .visual => "VISUAL"
    | .visualBlock => "VISUAL BLOCK"

structure BufferSyntaxState where
  lineSpans : Array (Nat × Array Syntax.Span) := #[]
  paletteOverrides : Syntax.Palette := {}
  deriving Repr, Inhabited

structure BufferState where
  id : Nat
  filename : Option String
  lines : Array String
  missingEol : Bool := false
  syntaxState : BufferSyntaxState := {}
  deriving Inhabited

structure FloatingOverlay where
  title : String
  lines : Array String
  maxWidth : Nat := 0
  cursorRow : Nat := 0
  cursorCol : Nat := 0
  deriving Repr, Inhabited

structure CompletionItem where
  label : String
  insertText : String
  deriving Repr, Inhabited

structure CompletionPopup where
  items : Array CompletionItem
  selected : Nat := 0
  anchorRow : Nat
  anchorCol : Nat
  deriving Repr, Inhabited

structure UiConfig where
  showLineNumbers : Bool := true
  vSplitStr : String := "│"
  hSplitStr : String := "─"
  emptyLineMarker : String := "~"
  statusBarStyle : String := ""
  resetStyle : String := "\x1b[0m"
  searchHighlightStyle : String := ""
  searchHighlightCursorStyle : String := ""
  visualSelectionStyle : String := "\x1b[7m"
  cursorCharStyle : String := "\x1b[47m\x1b[30m"
  cursorSpaceStyle : String := "\x1b[47m\x1b[30m"
  tabStop : Nat := 4
  syntaxPalette : Syntax.Palette := Syntax.defaultPalette
  deriving Inhabited

structure Model where
  mode : Mode
  workgroupName : String
  workspace : WorkspaceView
  buffers : Array BufferState
  selectionStart : Option Cursor := none
  message : String
  commandBuffer : String
  floatingOverlay : Option FloatingOverlay := none
  completionPopup : Option CompletionPopup := none
  config : UiConfig := {}
  windowHeight : Nat := 24
  windowWidth : Nat := 80
  dirty : Bool := true
  deriving Inhabited

def getBuffer (m : Model) (id : Nat) : BufferState :=
  m.buffers.find? (fun b => b.id == id) |>.getD { id := id, filename := none, lines := #[] }

end Bliku.Tui
