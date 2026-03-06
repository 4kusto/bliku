import Bliku.Tui.Primitives
import Bliku.Tui.Model
import Bliku.Tui.Window
import Bliku.Tui.Overlay
import Bliku.Tui.RenderInput
import Bliku.Tui.Terminal
import Bliku.Tui.Syntax
import Bliku.Decoration
import Bliku.Widget.TextView
import Bliku.Widget.Chrome

namespace Bliku.Tui

open Bliku.Tui.Terminal

private def getBufferLine (buf : BufferState) (row : Nat) : String :=
  buf.lines[row]?.getD ""

private def charDisplayWidth (ch : Char) : Nat :=
  let n := ch.toNat
  if
    (0x1100 <= n && n <= 0x115F) ||
    (0x2329 <= n && n <= 0x232A) ||
    (0x2E80 <= n && n <= 0xA4CF) ||
    (0xAC00 <= n && n <= 0xD7A3) ||
    (0xF900 <= n && n <= 0xFAFF) ||
    (0xFE10 <= n && n <= 0xFE19) ||
    (0xFE30 <= n && n <= 0xFE6F) ||
    (0xFF00 <= n && n <= 0xFF60) ||
    (0xFFE0 <= n && n <= 0xFFE6)
  then 2 else 1

private def lineDisplayLength (line : String) : Nat :=
  line.toList.foldl (fun acc ch => acc + charDisplayWidth ch) 0

private def takeDisplayWidth (s : String) (width : Nat) : String := Id.run do
  let mut out := ""
  let mut used := 0
  for ch in s.toList do
    let chWidth := charDisplayWidth ch
    if used + chWidth <= width then
      out := out.push ch
      used := used + chWidth
  out

private def padPlainLine (s : String) (width : Nat) : String :=
  let clipped := takeDisplayWidth s width
  let clippedWidth := lineDisplayLength clipped
  let pad := if clippedWidth < width then "".pushn ' ' (width - clippedWidth) else ""
  clipped ++ pad

def renderEmptyLineMarkerInRect (config : UiConfig) (width : Nat) : String :=
  padPlainLine config.emptyLineMarker width

private def renderedWindowLineWidth
    (line : String) (scrollCol availableWidth : Nat) (windowCursor : Option Cursor) : Nat :=
  let lineLen := lineDisplayLength line
  let visibleLen := if lineLen > scrollCol then min availableWidth (lineLen - scrollCol) else 0
  match windowCursor with
  | some cur =>
      if cur.col >= scrollCol then
        let visCol := cur.col - scrollCol
        if visibleLen <= visCol && visCol < availableWidth then
          visCol + 1
        else
          visibleLen
      else
        visibleLen
  | none => visibleLen

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

private def byteDecorationsForLine (m : Model) (buf : BufferState) (lineIdx : Nat) (line : String) : Array ByteDecoration :=
  let palette := syntaxPaletteFor m buf
  (syntaxSpansForLine buf lineIdx line).foldl (fun acc span =>
    match Syntax.Palette.faceFor palette span.kind with
    | some face =>
        acc.push {
          row := lineIdx
          startByte := span.startByte
          endByte := span.endByte
          priority := 10
          style := face.toAnsi
        }
            | none => acc) #[]

private def normalizeCursorRange (a b : Cursor) : Cursor × Cursor :=
  if a.row < b.row || (a.row == b.row && a.col <= b.col) then (a, b) else (b, a)

private def isInSelection (selection : SelectionState) (row col : Nat) : Bool :=
  if selection.block then
    let minRow := min selection.anchor.row selection.cursor.row
    let maxRow := max selection.anchor.row selection.cursor.row
    let minCol := min selection.anchor.col selection.cursor.col
    let maxCol := max selection.anchor.col selection.cursor.col
    row >= minRow && row <= maxRow && col >= minCol && col <= maxCol
  else
    let (p1, p2) := normalizeCursorRange selection.anchor selection.cursor
    if row < p1.row || row > p2.row then false
    else if row > p1.row && row < p2.row then true
    else if p1.row == p2.row then col >= p1.col && col <= p2.col
    else if row == p1.row then col >= p1.col
    else if row == p2.row then col <= p2.col
    else false

private def selectionDecorationsForLine
    (config : UiConfig) (selection : Option SelectionState) (lineIdx scrollCol availableWidth : Nat) : Array CellDecoration := Id.run do
  let some selection := selection | return #[]
  let mut startCol : Option Nat := none
  let mut endCol := scrollCol
  for col in [scrollCol:scrollCol + availableWidth] do
    if isInSelection selection lineIdx col then
      if startCol.isNone then
        startCol := some col
      endCol := col + 1
  match startCol with
  | some start =>
      #[{
        row := lineIdx
        startCol := start
        endCol := endCol
        priority := 100
        style := config.visualSelectionStyle
      }]
  | none => #[]

private def cursorDecorationForLine
    (m : Model) (windowId lineIdx : Nat) (windowCursor : Option Cursor) : Option CursorDecoration :=
  match windowCursor with
  | some cur =>
      if windowId == m.workspace.activeWindowId && lineIdx == cur.row then
        some {
          row := lineIdx
          col := cur.col
          charStyle := m.config.cursorCharStyle
          spaceStyle := m.config.cursorSpaceStyle
        }
      else
        none
  | none => none

def renderVisibleLine
    (m : Model) (input : RenderInput) (buf : BufferState) (windowId lineIdx scrollCol availableWidth : Nat)
    (windowCursor : Option Cursor) : String :=
  let line := getBufferLine buf lineIdx
  let props : Bliku.Widget.TextViewProps := {
    lines := #[line]
    scrollCol := scrollCol
    byteDecorations := (byteDecorationsForLine m buf lineIdx line).map (fun deco => { deco with row := 0 })
    cellDecorations := (selectionDecorationsForLine m.config input.selection lineIdx scrollCol availableWidth).map (fun deco =>
      { deco with row := 0 })
    cursor := (cursorDecorationForLine m windowId lineIdx windowCursor).map (fun deco => { deco with row := 0 })
    resetStyle := m.config.resetStyle
  }
  let styled := Bliku.Widget.renderVisibleLine props 0 availableWidth
  Bliku.Widget.appendCursorOnBlankCell props 0 availableWidth styled

def renderVisibleLineInRect
    (m : Model) (input : RenderInput) (buf : BufferState) (windowId lineIdx scrollCol availableWidth : Nat)
    (windowCursor : Option Cursor) : String :=
  let line := getBufferLine buf lineIdx
  let rendered := renderVisibleLine m input buf windowId lineIdx scrollCol availableWidth windowCursor
  let occupied := renderedWindowLineWidth line scrollCol availableWidth windowCursor
  let pad := if occupied < availableWidth then "".pushn ' ' (availableWidth - occupied) else ""
  rendered ++ pad

private def renderWindow (m : Model) (input : RenderInput) (windowId : Nat) (view : ViewState) (rect : Rect) : (Array String × Option (Nat × Nat)) := Id.run do
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
    let lnWidth := if m.config.showLineNumbers then 4 else 0
    let availableWidth := if w > lnWidth then w - lnWidth else 0
    if lineIdx < buf.lines.size then
      let windowCursor := if windowId == m.workspace.activeWindowId then some view.cursor else none
      if m.config.showLineNumbers then
        out := out.push s!"{leftPad (toString (lineIdx + 1)) 3} "
      out := out.push (renderVisibleLineInRect m input buf windowId lineIdx view.scrollCol availableWidth windowCursor)
    else
      out := out.push (renderEmptyLineMarkerInRect m.config w)

  let statusRow := r + workH
  out := out.push (moveCursorStr statusRow c)
  out := out.push m.config.statusBarStyle
  out := out.push (padPlainLine input.statusLine w)
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

private def renderLayout (m : Model) (input : RenderInput) (l : Layout) (r c h w : Nat) (skipFloating : Bool := true) : (Array String × Option (Nat × Nat)) := Id.run do
  match l with
  | .group _ body =>
    return renderLayout m input body r c h w skipFloating
  | .pane id view =>
    if skipFloating && m.workspace.isFloatingWindow id then
      let mut blank : Array String := #[]
      for i in [0:h] do
        blank := blank.push (moveCursorStr (r + i) c)
        blank := blank.push ("".pushn ' ' w)
      return (blank, none)
    let (buf, cur) := renderWindow m input id view { row := r, col := c, height := h, width := w }
    return (buf, if id == m.workspace.activeWindowId then cur else none)
  | .hsplit left right ratio =>
    let leftW := (Float.ofNat w * ratio).toUInt64.toNat
    let (leftBuf, leftCur) := renderLayout m input left r c h leftW skipFloating
    let mut sep : Array String := #[]
    if w > leftW then
      let sepCol := c + leftW
      for i in [0:h] do
        sep := sep.push (moveCursorStr (r + i) sepCol)
        sep := sep.push m.config.vSplitStr
    let (rightBuf, rightCur) := renderLayout m input right r (c + leftW + 1) h (if w > leftW then w - leftW - 1 else 0) skipFloating
    return (leftBuf ++ sep ++ rightBuf, rightCur.orElse (fun _ => leftCur))
  | .vsplit top bottom ratio =>
    let topH := (Float.ofNat h * ratio).toUInt64.toNat
    let (topBuf, topCur) := renderLayout m input top r c topH w skipFloating
    let mut sep : Array String := #[]
    if h > topH then
      let sepRow := r + topH
      sep := sep.push (moveCursorStr sepRow c)
      for _ in [0:w] do
        sep := sep.push m.config.hSplitStr
    let (bottomBuf, bottomCur) := renderLayout m input bottom (r + topH + 1) c (if h > topH then h - topH - 1 else 0) w skipFloating
    return (topBuf ++ sep ++ bottomBuf, bottomCur.orElse (fun _ => topCur))

private def renderFloatingClusters (m : Model) (input : RenderInput) : (Array String × Option (Nat × Nat)) := Id.run do
  let ws := m.workspace
  let mut out : Array String := #[]
  let mut activeCur : Option (Nat × Nat) := none
  let clusters := ws.floatingClusters
  for i in [0:clusters.size] do
    let cluster := clusters[i]!
    match ws.layout.extractFloatingRoot cluster.root,
      computeFloatingClusterBounds m.windowHeight m.windowWidth i cluster with
    | some subtree, some (baseTop, baseLeft, h, w) =>
      let toNatNonNeg (v : Int) := if v < 0 then 0 else Int.toNat v
      let availableRows := if m.windowHeight > 1 then m.windowHeight - 1 else m.windowHeight
      let maxTop := if availableRows > h then availableRows - h else 0
      let maxLeft := if m.windowWidth > w then m.windowWidth - w else 0
      let top := min maxTop (toNatNonNeg (Int.ofNat baseTop + cluster.rowOffset))
      let left := min maxLeft (toNatNonNeg (Int.ofNat baseLeft + cluster.colOffset))
      let isActive := subtree.getPaneIds.contains ws.activeWindowId
      let chromeBox := Bliku.Widget.renderFloatingChromeBox
        cluster.chrome w h
        m.config.floatingChromeActiveStyle
        m.config.floatingChromeInactiveStyle
        m.config.resetStyle
        isActive
      if !chromeBox.lines.isEmpty then
        for j in [0:chromeBox.lines.size] do
          out := out.push (moveCursorStr (top + j) left)
          out := out.push chromeBox.lines[j]!
      let innerTop := top + chromeBox.insetTop
      let innerLeft := left + chromeBox.insetLeft
      let innerH := h - chromeBox.insetTop - chromeBox.insetBottom
      let innerW := w - chromeBox.insetLeft - chromeBox.insetRight
      if innerH > 0 && innerW > 0 then
        let (buf, cur) := renderLayout m input subtree innerTop innerLeft innerH innerW false
        out := out ++ buf
        if isActive then
          activeCur := cur
    | _, _ => pure ()
  (out, activeCur)

private def findOverlayCursorPos (rows cols : Nat) (input : RenderInput) : Option (Nat × Nat) :=
  match input.overlay with
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
def renderWith (model : Model) (input : RenderInput) : IO Model := do
  if !model.dirty then
    return model

  let (rows, cols) ← getWindowSize
  let m := { model with windowHeight := rows, windowWidth := cols }
  let mut buffer : Array String := #[]
  buffer := buffer.push hideCursorStr

  let layoutH := if rows > 0 then rows - 1 else 0
  let baseLayout :=
    m.workspace.floatingClusters.foldl (fun acc cluster =>
      match acc with
      | some l => l.removeFloatingRoot cluster.root
      | none => none) (some m.workspace.layout)

  if baseLayout.isNone then
    buffer := buffer.push clearScreenStr
  buffer := buffer.push homeCursorStr

  let (layoutBuf, activeCur) :=
    match baseLayout with
    | some l => renderLayout m input l 0 0 layoutH cols true
    | none => (#[], none)
  buffer := buffer ++ layoutBuf

  let (floatBuf, floatCur) := renderFloatingClusters m input
  buffer := buffer ++ floatBuf

  buffer := buffer.push (moveCursorStr (rows - 1) 0)
  buffer := buffer.push (padPlainLine (renderStatusBar input) cols)

  let overlayToRender := input.overlay.orElse (fun _ => messageOverlayForState input)
  if let some ov := overlayToRender then
    buffer := buffer ++ renderFloatingOverlay rows cols ov

  if let some popup := input.completion then
    buffer := buffer ++ renderCompletionPopup rows cols m.config popup activeCur

  let overlayCur := findOverlayCursorPos rows cols input
  buffer := buffer.push (
    if input.overlay.isSome && input.commandLine.isNone then
      match overlayCur with
      | some (pr, pc) => moveCursorStr pr pc
      | none => ""
    else
      match floatCur.orElse (fun _ => activeCur) with
      | some (pr, pc) => moveCursorStr pr pc
      | none => ""
  )

  if let some prompt := input.commandLine then
    buffer := buffer.push (moveCursorStr (rows - 1) (Bliku.Widget.promptCursorCol { leader := prompt.leader, text := prompt.text, cursorCol := prompt.cursorCol }))

  if input.hideTerminalCursor then
    buffer := buffer.push hideCursorStr
  else
    buffer := buffer.push showCursorStr

  IO.print (String.intercalate "" buffer.toList)
  (← IO.getStdout).flush
  return m

def render (model : Model) : IO Model :=
  renderWith model {}

end Bliku.Tui
