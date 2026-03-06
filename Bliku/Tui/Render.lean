import Bliku.Tui.Primitives
import Bliku.Tui.Model
import Bliku.Tui.Window
import Bliku.Tui.Overlay
import Bliku.Tui.Terminal
import Bliku.Tui.Syntax

namespace Bliku.Tui

open Bliku.Tui.Terminal

structure VisibleCell where
  text : String
  col : Nat
  startByte : Nat
  endByte : Nat
  deriving Repr, Inhabited

private def collectVisibleCells (line : String) (scrollCol width : Nat) : Array VisibleCell := Id.run do
  let mut out : Array VisibleCell := #[]
  let mut bytePos := 0
  let mut col := 0
  for ch in line.toList do
    let nextByte := bytePos + ch.utf8Size
    if scrollCol <= col && col < scrollCol + width then
      out := out.push {
        text := ch.toString
        col := col
        startByte := bytePos
        endByte := nextByte
      }
    bytePos := nextByte
    col := col + 1
  out

private def lineDisplayLength (line : String) : Nat :=
  line.toList.length

private def getBufferLine (buf : BufferState) (row : Nat) : String :=
  buf.lines[row]?.getD ""

private def getExternalSyntaxLine (buf : BufferState) (lineIdx : Nat) : Array Syntax.Span :=
  match buf.syntaxState.lineSpans.find? (fun (row, _) => row == lineIdx) with
  | some (_, spans) => spans
  | none => #[]

private def syntaxPaletteFor (m : Model) (buf : BufferState) : Syntax.Palette :=
  Syntax.Palette.merge m.config.syntaxPalette buf.syntaxState.paletteOverrides

private def syntaxSpansForLine (buf : BufferState) (lineIdx : Nat) (line : String) : Array Syntax.Span :=
  let builtin := Syntax.highlightLine buf.filename line
  let external := getExternalSyntaxLine buf lineIdx
  external ++ builtin

private def isVisualMode (mode : Mode) : Bool :=
  mode == .visual || mode == .visualBlock

private def normalizeCursorRange (a b : Cursor) : Cursor × Cursor :=
  if a.row < b.row || (a.row == b.row && a.col <= b.col) then (a, b) else (b, a)

private def isInSelection (m : Model) (cur : Cursor) (row col : Nat) : Bool :=
  if !isVisualMode m.mode then
    false
  else
    match m.selectionStart with
    | none => false
    | some start =>
      if m.mode == .visualBlock then
        let minRow := min start.row cur.row
        let maxRow := max start.row cur.row
        let minCol := min start.col cur.col
        let maxCol := max start.col cur.col
        row >= minRow && row <= maxRow && col >= minCol && col <= maxCol
      else
        let (p1, p2) := normalizeCursorRange start cur
        if row < p1.row || row > p2.row then false
        else if row > p1.row && row < p2.row then true
        else if p1.row == p2.row then col >= p1.col && col <= p2.col
        else if row == p1.row then col >= p1.col
        else if row == p2.row then col <= p2.col
        else false

def renderVisibleLine
    (m : Model) (buf : BufferState) (windowId lineIdx scrollCol availableWidth : Nat)
    (windowCursor : Option Cursor) : String := Id.run do
  let activeRow := windowCursor.map (fun c => c.row) |>.getD 0
  let activeCol := windowCursor.map (fun c => c.col) |>.getD 0
  let selectionCur := windowCursor.getD { row := lineIdx, col := scrollCol }
  let line := getBufferLine buf lineIdx
  let visibleCells := collectVisibleCells line scrollCol availableWidth
  let syntaxSpans := syntaxSpansForLine buf lineIdx line
  let syntaxPalette := syntaxPaletteFor m buf
  let mut out : Array String := #[]
  let mut activeStyle : Option String := none
  for cell in visibleCells do
    let isCursor :=
      windowCursor.isSome &&
      windowId == m.workspace.activeWindowId &&
      lineIdx == activeRow &&
      cell.col == activeCol
    let desiredStyle : Option String :=
      if isCursor then
        if cell.text == " " then some m.config.cursorSpaceStyle else some m.config.cursorCharStyle
      else if isInSelection m selectionCur lineIdx cell.col then
        some m.config.visualSelectionStyle
      else
        (Syntax.faceForByteRange syntaxPalette syntaxSpans cell.startByte cell.endByte).map Syntax.Face.toAnsi
    if desiredStyle != activeStyle then
      match activeStyle with
      | some _ => out := out.push m.config.resetStyle
      | none => pure ()
      match desiredStyle with
      | some stl => out := out.push stl
      | none => pure ()
      activeStyle := desiredStyle
    out := out.push cell.text
  if activeStyle.isSome then
    out := out.push m.config.resetStyle
  return String.intercalate "" out.toList

private def appendCursorOnBlankCell
    (m : Model) (windowId lineIdx scrollCol availableWidth : Nat)
    (line : String) (lineToDraw : String) (windowCursor : Option Cursor) : String := Id.run do
  let some cur := windowCursor | return lineToDraw
  if windowId != m.workspace.activeWindowId || lineIdx != cur.row || cur.col < scrollCol then
    return lineToDraw
  let visCol := cur.col - scrollCol
  let visLen := lineDisplayLength line - scrollCol |> min availableWidth
  if visCol < visLen || visCol >= availableWidth then
    return lineToDraw
  let padCount := visCol - visLen
  return lineToDraw ++ ("".pushn ' ' padCount) ++ m.config.cursorSpaceStyle ++ " " ++ m.config.resetStyle

private def renderWindow (m : Model) (windowId : Nat) (view : ViewState) (rect : Rect) : (Array String × Option (Nat × Nat)) := Id.run do
  let r := rect.row
  let c := rect.col
  let h := rect.height
  let w := rect.width
  let workH := if h > 0 then h - 1 else 0
  let buf := getBuffer m view.bufferId
  let mut out : Array String := #[]

  for i in [0:workH] do
    let lineIdx := view.scrollRow + i
    out := out.push (moveCursorStr (r + i) c)
    if lineIdx < buf.lines.size then
      let lnWidth := if m.config.showLineNumbers then 4 else 0
      let availableWidth := if w > lnWidth then w - lnWidth else 0
      let windowCursor := if windowId == m.workspace.activeWindowId then some view.cursor else none
      if m.config.showLineNumbers then
        out := out.push s!"{leftPad (toString (lineIdx + 1)) 3} "
      let line := getBufferLine buf lineIdx
      let styled := renderVisibleLine m buf windowId lineIdx view.scrollCol availableWidth windowCursor
      let lineToDraw := appendCursorOnBlankCell m windowId lineIdx view.scrollCol availableWidth line styled windowCursor
      out := out.push lineToDraw
    else
      out := out.push m.config.emptyLineMarker
    out := out.push clearLineStr

  let statusRow := r + workH
  out := out.push (moveCursorStr statusRow c)
  let fileName := buf.filename.getD "[No Name]"
  let modeStr := if windowId == m.workspace.activeWindowId then s!"-- {m.mode} --" else "--"
  let eolMark := if buf.missingEol then " [noeol]" else ""
  let statusStr := s!"{modeStr} {fileName}{eolMark} [W:{windowId} B:{view.bufferId}] [{m.workgroupName}] {m.workspace.name}"
  out := out.push m.config.statusBarStyle
  out := out.push statusStr
  out := out.push clearLineStr
  out := out.push m.config.resetStyle

  let cursorPos : Option (Nat × Nat) :=
    if view.cursor.row < view.scrollRow then none
    else
      let visRow := view.cursor.row - view.scrollRow
      if visRow < workH then
        let colOffset := if m.config.showLineNumbers then 4 else 0
        let visCol := if view.cursor.col >= view.scrollCol then view.cursor.col - view.scrollCol else 0
        if visCol + colOffset < w then
          some (r + visRow, c + visCol + colOffset)
        else none
      else none

  (out, cursorPos)

private def renderLayout (m : Model) (l : Layout) (r c h w : Nat) : (Array String × Option (Nat × Nat)) := Id.run do
  match l with
  | .window id view =>
    if m.workspace.isFloatingWindow id then
      let mut blank : Array String := #[]
      for i in [0:h] do
        blank := blank.push (moveCursorStr (r + i) c)
        blank := blank.push ("".pushn ' ' w)
      return (blank, none)
    let (buf, cur) := renderWindow m id view { row := r, col := c, height := h, width := w }
    return (buf, if id == m.workspace.activeWindowId then cur else none)
  | .hsplit left right ratio =>
    let leftW := (Float.ofNat w * ratio).toUInt64.toNat
    let (leftBuf, leftCur) := renderLayout m left r c h leftW
    let mut sep : Array String := #[]
    if w > leftW then
      let sepCol := c + leftW
      for i in [0:h] do
        sep := sep.push (moveCursorStr (r + i) sepCol)
        sep := sep.push m.config.vSplitStr
    let (rightBuf, rightCur) := renderLayout m right r (c + leftW + 1) h (if w > leftW then w - leftW - 1 else 0)
    return (leftBuf ++ sep ++ rightBuf, rightCur.orElse (fun _ => leftCur))
  | .vsplit top bottom ratio =>
    let topH := (Float.ofNat h * ratio).toUInt64.toNat
    let (topBuf, topCur) := renderLayout m top r c topH w
    let mut sep : Array String := #[]
    if h > topH then
      let sepRow := r + topH
      sep := sep.push (moveCursorStr sepRow c)
      for _ in [0:w] do
        sep := sep.push m.config.hSplitStr
    let (bottomBuf, bottomCur) := renderLayout m bottom (r + topH + 1) c (if h > topH then h - topH - 1 else 0) w
    return (topBuf ++ sep ++ bottomBuf, bottomCur.orElse (fun _ => topCur))

private def renderFloatingWindows (m : Model) : (Array String × Option (Nat × Nat)) := Id.run do
  let ws := m.workspace
  let mut out : Array String := #[]
  let mut activeCur : Option (Nat × Nat) := none
  let ids := ws.getFloatingWindowIds
  for i in [0:ids.size] do
    let wid := ids[i]!
    match ws.layout.findView wid, getFloatingWindowBounds ws m.windowHeight m.windowWidth wid with
    | some view, some (top, left, h, w) =>
      let (buf, cur) := renderWindow m wid view { row := top, col := left, height := h, width := w }
      out := out ++ buf
      if wid == ws.activeWindowId then
        activeCur := cur
    | _, _ => pure ()
  (out, activeCur)

private def findOverlayCursorPos (rows cols : Nat) (m : Model) : Option (Nat × Nat) :=
  match m.floatingOverlay with
  | none => none
  | some ov =>
    match computeFloatingOverlayLayout rows cols ov with
    | none => none
    | some layout =>
      let lines := if ov.lines.isEmpty then #[""] else ov.lines
      let maxRow := lines.size - 1
      let rowIdx := min ov.cursorRow maxRow
      let lineLen := (lines[rowIdx]!).length
      let colIdx := min ov.cursorCol lineLen
      let visRow := min rowIdx (layout.contentRows - 1)
      let visCol := min colIdx layout.innerWidth
      some (layout.top + 1 + layout.titleRows + visRow, layout.left + 2 + visCol)

/-- Render model to terminal and return updated model (size synchronized). -/
def render (model : Model) : IO Model := do
  if !model.dirty then
    return model

  let (rows, cols) ← getWindowSize
  let m := { model with windowHeight := rows, windowWidth := cols }
  let mut buffer : Array String := #[]
  buffer := buffer.push hideCursorStr

  let layoutH := if rows > 0 then rows - 1 else 0
  let baseLayout :=
    m.workspace.getFloatingWindowIds.foldl (fun acc wid =>
      match acc with
      | some l => l.remove wid
      | none => none) (some m.workspace.layout)

  if baseLayout.isNone then
    buffer := buffer.push clearScreenStr
  buffer := buffer.push homeCursorStr

  let (layoutBuf, activeCur) :=
    match baseLayout with
    | some l => renderLayout m l 0 0 layoutH cols
    | none => (#[], none)
  buffer := buffer ++ layoutBuf

  let (floatBuf, floatCur) := renderFloatingWindows m
  buffer := buffer ++ floatBuf

  buffer := buffer.push (moveCursorStr (rows - 1) 0)
  buffer := buffer.push (renderStatusBar m)
  buffer := buffer.push clearLineStr

  let overlayToRender := m.floatingOverlay.orElse (fun _ => messageOverlayForState m)
  if let some ov := overlayToRender then
    buffer := buffer ++ renderFloatingOverlay rows cols ov

  buffer := buffer ++ renderCompletionPopup rows cols m activeCur

  let overlayCur := findOverlayCursorPos rows cols m
  buffer := buffer.push (
    if m.floatingOverlay.isSome && m.mode != .command && m.mode != .searchForward && m.mode != .searchBackward then
      match overlayCur with
      | some (pr, pc) => moveCursorStr pr pc
      | none => ""
    else
      match floatCur.orElse (fun _ => activeCur) with
      | some (pr, pc) => moveCursorStr pr pc
      | none => ""
  )

  if m.mode == .command || m.mode == .searchForward || m.mode == .searchBackward then
    buffer := buffer.push (moveCursorStr (rows - 1) (1 + m.commandBuffer.length))

  if m.mode == .visual || m.mode == .visualBlock then
    buffer := buffer.push hideCursorStr
  else
    buffer := buffer.push showCursorStr

  IO.print (String.intercalate "" buffer.toList)
  (← IO.getStdout).flush
  return m

end Bliku.Tui
