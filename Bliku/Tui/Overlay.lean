import Bliku.Tui.Primitives
import Bliku.Tui.Model
import Bliku.Tui.RenderInput
import Bliku.Tui.Terminal
import Bliku.Widget.Popup
import Bliku.Widget.List
import Bliku.Widget.Prompt

namespace Bliku.Tui

open Bliku.Tui.Terminal

def shouldRenderMessageAsFloat (msg : String) : Bool :=
  let m := msg.trimAscii.toString
  if m.isEmpty then
    false
  else
    m.startsWith "Error" ||
    m.startsWith "Cannot" ||
    m.startsWith "Invalid" ||
    m.startsWith "Unknown" ||
    m.startsWith "No " ||
    m.startsWith "Empty " ||
    m.startsWith "Usage:" ||
    m.startsWith "failed" ||
    m.startsWith "Failed" ||
    m.contains "not found"

def renderStatusBar (input : RenderInput) : String :=
  let plain := input.messageLine.trimAscii.toString
  let floatMsg := shouldRenderMessageAsFloat plain
  match input.commandLine with
  | some cmd => Bliku.Widget.renderPrompt { leader := cmd.leader, text := cmd.text, cursorCol := cmd.cursorCol }
  | none => if floatMsg then input.statusLine else plain

structure FloatingOverlayLayout where
  top : Nat
  left : Nat
  innerWidth : Nat
  titleRows : Nat
  contentRows : Nat
  deriving Inhabited

def computeFloatingOverlayLayout (rows cols : Nat) (overlay : OverlayView) : Option FloatingOverlayLayout := Id.run do
  let availableRows := if rows > 1 then rows - 1 else rows
  if cols < 8 || availableRows < 4 then
    return none
  let box := Bliku.Widget.renderPopupBox { title := overlay.title, lines := overlay.lines, maxWidth := overlay.maxWidth }
  let maxInnerWidth := if cols > 8 then cols - 8 else 1
  let innerWidth := max 1 (min box.innerWidth maxInnerWidth)
  let maxContentRows := if availableRows > box.titleRows + 2 then availableRows - box.titleRows - 2 else 0
  if maxContentRows == 0 then return none
  let contentRows := max 1 (min box.contentRows maxContentRows)
  let boxHeight := contentRows + box.titleRows + 2
  let boxWidth := innerWidth + 4
  let top := (availableRows - boxHeight) / 2
  let left := (cols - boxWidth) / 2
  return some { top, left, innerWidth, titleRows := box.titleRows, contentRows }

def renderFloatingOverlay (rows cols : Nat) (overlay : OverlayView) : Array String := Id.run do
  let some layout := computeFloatingOverlayLayout rows cols overlay | return #[]
  let popup := Bliku.Widget.renderPopupBox { title := overlay.title, lines := overlay.lines, maxWidth := layout.innerWidth }
  let mut out : Array String := #[]
  for i in [0:popup.lines.size] do
    out := out.push (moveCursorStr (layout.top + i) layout.left)
    out := out.push popup.lines[i]!
  out

def messageOverlayForState (input : RenderInput) : Option OverlayView :=
  let plain := input.messageLine.trimAscii.toString
  let floatMsg := shouldRenderMessageAsFloat plain
  if input.overlay.isNone && input.commandLine.isNone && floatMsg then
    if plain.isEmpty then none
    else some { title := "Message", lines := (plain.splitOn "\n").toArray }
  else
    none

def renderCompletionPopup (_rows cols : Nat) (config : UiConfig) (popup : CompletionView) (cursorPos : Option (Nat × Nat)) : Array String := Id.run do
  let some (cursorRow, cursorCol) := cursorPos | return #[]
  let items := popup.items.map (fun it => Bliku.Widget.ListItem.mk it.label)
  let borderCh := if config.hSplitStr.isEmpty then '-' else config.hSplitStr.toList[0]!
  let box := Bliku.Widget.renderListBox {
    items := items
    selected := popup.selected
    maxVisible := 8
    borderLeft := config.vSplitStr
    borderRight := config.vSplitStr
    borderFill := borderCh
    selectedStyle := "\x1b[7m"
    resetStyle := config.resetStyle
  }
  if box.lines.isEmpty then
    return #[]
  let top := cursorRow + 1
  let left := if cursorCol + box.width < cols then cursorCol else (cols - box.width)
  let mut out : Array String := #[]
  for i in [0:box.lines.size] do
    out := out.push (moveCursorStr (top + i) left)
    out := out.push box.lines[i]!
  out

end Bliku.Tui
