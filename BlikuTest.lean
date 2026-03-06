import Bliku

open Bliku.Tui

private def mkWorkspace : WorkspaceView :=
  {
    name := "test"
    layout := .window 0 { bufferId := 0, cursor := { row := 0, col := 0 }, scrollRow := 0, scrollCol := 0 }
    activeWindowId := 0
  }

private def mkModel (mode : Mode := .normal) (selectionStart : Option Cursor := none) : Model :=
  {
    mode := mode
    workgroupName := "wg"
    workspace := mkWorkspace
    buffers := #[]
    selectionStart := selectionStart
    message := ""
    commandBuffer := ""
    config := {
      resetStyle := "[/]"
      visualSelectionStyle := "[SEL]"
      cursorCharStyle := "[CUR]"
      cursorSpaceStyle := "[CUR]"
    }
  }

private def assertEq [ToString α] [BEq α] (name : String) (actual expected : α) : IO Unit := do
  if actual != expected then
    throw <| IO.userError s!"{name} failed\nexpected: {expected}\nactual:   {actual}"

private def utf8ByteOffsetTest : IO Unit := do
  let buf : BufferState :=
    {
      id := 0
      filename := none
      lines := #["あdef"]
      syntaxState := {
        lineSpans := #[(0, #[{ startByte := 3, endByte := 6, kind := .custom "test" }])]
        paletteOverrides := { custom := [("test", { fg := some (.ansi 196) })] }
      }
    }
  let rendered := renderVisibleLine (mkModel) buf 0 0 0 4 (some { row := 1, col := 0 })
  assertEq "utf8-byte-offset" rendered "あ\x1b[38;5;196mdef[/]"

private def builtinLeanHighlightTest : IO Unit := do
  let buf : BufferState :=
    {
      id := 0
      filename := some "Main.lean"
      lines := #["def あ := 1"]
    }
  let rendered := renderVisibleLine (mkModel) buf 0 0 0 20 none
  assertEq "builtin-lean-highlight" rendered "\x1b[38;5;12mdef[/] あ := \x1b[38;5;13m1[/]"

private def cursorSelectionPriorityTest : IO Unit := do
  let model := mkModel .visual (some { row := 0, col := 0 })
  let buf : BufferState :=
    {
      id := 0
      filename := none
      lines := #["abc"]
      syntaxState := {
        lineSpans := #[(0, #[{ startByte := 0, endByte := 3, kind := .custom "test" }])]
        paletteOverrides := { custom := [("test", { fg := some (.ansi 46) })] }
      }
    }
  let rendered := renderVisibleLine model buf 0 0 0 3 (some { row := 0, col := 1 })
  assertEq "cursor-selection-priority" rendered "[SEL]a[/][CUR]b[/]\x1b[38;5;46mc[/]"

def main : IO Unit := do
  utf8ByteOffsetTest
  builtinLeanHighlightTest
  cursorSelectionPriorityTest
  IO.println "bliku-test: ok"
