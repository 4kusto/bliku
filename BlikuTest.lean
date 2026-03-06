import Bliku

open Bliku.Tui

private def mkWorkspace : WorkspaceView :=
  {
    name := "test"
    desktop := {
      layout := .pane 0 { contentId := 0, cursor := { row := 0, col := 0 }, scrollRow := 0, scrollCol := 0 }
      activePaneId := 0
    }
  }

private def mkModel : Model :=
  {
    workspace := mkWorkspace
    buffers := #[]
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
  let rendered := renderVisibleLine mkModel {} buf 0 0 0 4 (some { row := 1, col := 0 })
  assertEq "utf8-byte-offset" rendered "あ\x1b[38;5;196mdef[/]"

private def builtinLeanHighlightTest : IO Unit := do
  let buf : BufferState :=
    {
      id := 0
      filename := some "Main.lean"
      lines := #["def あ := 1"]
    }
  let rendered := renderVisibleLine mkModel {} buf 0 0 0 20 none
  assertEq "builtin-lean-highlight" rendered "\x1b[38;5;12mdef[/] あ := \x1b[38;5;13m1[/]"

private def cursorSelectionPriorityTest : IO Unit := do
  let input : RenderInput := {
    selection := some { anchor := { row := 0, col := 0 }, cursor := { row := 0, col := 1 } }
  }
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
  let rendered := renderVisibleLine mkModel input buf 0 0 0 3 (some { row := 0, col := 1 })
  assertEq "cursor-selection-priority" rendered "[SEL]a[/][CUR]b[/]\x1b[38;5;46mc[/]"

private def floatingClusterLayoutTest : IO Unit := do
  let layout : Bliku.PaneLayout :=
    .hsplit
      (.pane 0 { contentId := 0, cursor := { row := 0, col := 0 }, scrollRow := 0, scrollCol := 0 })
      (.group 100 <|
        .vsplit
          (.pane 1 { contentId := 1, cursor := { row := 0, col := 0 }, scrollRow := 0, scrollCol := 0 })
          (.pane 2 { contentId := 2, cursor := { row := 0, col := 0 }, scrollRow := 0, scrollCol := 0 })
          0.5)
      0.5
  let desktop : Bliku.DesktopLayout := {
    layout := layout
    activePaneId := 2
    floating := {
      clusters := #[{ root := .group 100, rowOffset := 1, colOffset := 2 }]
    }
  }
  let ws : WorkspaceView := { name := "test", desktop := desktop }
  assertEq "floating-cluster-pane-1" (ws.isFloatingWindow 1) true
  assertEq "floating-cluster-pane-2" (ws.isFloatingWindow 2) true
  assertEq "floating-cluster-pane-0" (ws.isFloatingWindow 0) false
  let extracted := layout.extractFloatingRoot (.group 100) |>.map Bliku.PaneLayout.getPaneIds
  assertEq "floating-cluster-extract" extracted (some [1, 2])
  let removed := layout.removeFloatingRoot (.group 100) |>.map Bliku.PaneLayout.getPaneIds
  assertEq "floating-cluster-remove" removed (some [0])

private def floatingSizePolicyTest : IO Unit := do
  let defaultBounds :=
    Bliku.computeFloatingClusterBounds 50 160 0 { root := .pane 1, sizePolicy := .default }
  let multiBounds :=
    Bliku.computeFloatingClusterBounds 50 160 0 { root := .group 100, sizePolicy := .multiPane }
  let customBounds :=
    Bliku.computeFloatingClusterBounds 50 160 0 {
      root := .group 200
      sizePolicy := .custom {
        width := .fixed 120
        height := .ratio 0.5
        minWidth := some 80
        maxHeight := some 30
      }
    }
  let some (_, _, defaultH, defaultW) := defaultBounds
    | throw <| IO.userError "default bounds missing"
  let some (_, _, multiH, multiW) := multiBounds
    | throw <| IO.userError "multi bounds missing"
  let some (_, _, customH, customW) := customBounds
    | throw <| IO.userError "custom bounds missing"
  assertEq "floating-size-multipane-wider" (decide (multiW > defaultW)) true
  assertEq "floating-size-multipane-taller" (decide (multiH > defaultH)) true
  assertEq "floating-size-custom-width" customW 120
  assertEq "floating-size-custom-height" customH 24

private def floatingChromeBoxTest : IO Unit := do
  let box := Bliku.Widget.renderFloatingChromeBox
    { kind := .bordered, title := some "Explorer" }
    16 4
    "[A]" "[I]" "[/]"
    true
  assertEq "floating-chrome-line-count" box.lines.size 4
  assertEq "floating-chrome-inset-top" box.insetTop 1
  assertEq "floating-chrome-inset-left" box.insetLeft 1
  assertEq "floating-chrome-top-line" box.lines[0]! "[A]┌ Explorer ────┐[/]"

private def rectSafeLineRenderTest : IO Unit := do
  let buf : BufferState := {
    id := 0
    filename := none
    lines := #["abc"]
  }
  let rendered := renderVisibleLineInRect mkModel {} buf 0 0 0 5 none
  assertEq "rect-safe-line-render" rendered "abc  "
  assertEq "rect-safe-line-no-clear" (rendered.contains '\x1b' && rendered.contains 'K') false

private def eofMarkerPlacementTest : IO Unit := do
  let marker := renderEmptyLineMarkerInRect { emptyLineMarker := "・" } 6
  assertEq "eof-marker-placement" marker "・    "

def main : IO Unit := do
  utf8ByteOffsetTest
  builtinLeanHighlightTest
  cursorSelectionPriorityTest
  floatingClusterLayoutTest
  floatingSizePolicyTest
  floatingChromeBoxTest
  rectSafeLineRenderTest
  eofMarkerPlacementTest
  IO.println "bliku-test: ok"
