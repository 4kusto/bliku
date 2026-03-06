import Bliku.Decoration

namespace Bliku.Widget

open Bliku

structure TextViewProps where
  lines : Array String
  scrollCol : Nat := 0
  byteDecorations : Array ByteDecoration := #[]
  cellDecorations : Array CellDecoration := #[]
  cursor : Option CursorDecoration := none
  resetStyle : String := "\x1b[0m"
  deriving Inhabited

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

private def faceForByteRange
    (decos : Array ByteDecoration) (row byteStart byteEnd : Nat) : Option String :=
  let rec loop (i : Nat) (best : Option ByteDecoration) : Option String :=
    if i >= decos.size then
      best.map ByteDecoration.style
    else
      let deco := decos[i]!
      let best' :=
        if deco.row == row && byteStart < deco.endByte && byteEnd > deco.startByte then
          match best with
          | none => some deco
          | some current => if current.priority <= deco.priority then some deco else best
        else
          best
      loop (i + 1) best'
  loop 0 none

private def faceForCell
    (decos : Array CellDecoration) (row col : Nat) : Option String :=
  let rec loop (i : Nat) (best : Option CellDecoration) : Option String :=
    if i >= decos.size then
      best.map CellDecoration.style
    else
      let deco := decos[i]!
      let best' :=
        if deco.row == row && deco.startCol <= col && col < deco.endCol then
          match best with
          | none => some deco
          | some current => if current.priority <= deco.priority then some deco else best
        else
          best
      loop (i + 1) best'
  loop 0 none

def renderVisibleLine (props : TextViewProps) (row width : Nat) : String := Id.run do
  let line := props.lines[row]?.getD ""
  let visibleCells := collectVisibleCells line props.scrollCol width
  let mut out : Array String := #[]
  let mut activeStyle : Option String := none
  for cell in visibleCells do
    let desiredStyle : Option String :=
      match props.cursor with
      | some cur =>
          if cur.row == row && cur.col == cell.col then
            some (if cell.text == " " then cur.spaceStyle else cur.charStyle)
          else
            (faceForCell props.cellDecorations row cell.col).orElse
              (fun _ => faceForByteRange props.byteDecorations row cell.startByte cell.endByte)
      | none =>
          (faceForCell props.cellDecorations row cell.col).orElse
            (fun _ => faceForByteRange props.byteDecorations row cell.startByte cell.endByte)
    if desiredStyle != activeStyle then
      match activeStyle with
      | some _ => out := out.push props.resetStyle
      | none => pure ()
      match desiredStyle with
      | some style => out := out.push style
      | none => pure ()
      activeStyle := desiredStyle
    out := out.push cell.text
  if activeStyle.isSome then
    out := out.push props.resetStyle
  return String.intercalate "" out.toList

def appendCursorOnBlankCell (props : TextViewProps) (row width : Nat) (lineToDraw : String) : String := Id.run do
  let some cur := props.cursor | return lineToDraw
  if cur.row != row || cur.col < props.scrollCol then
    return lineToDraw
  let line := props.lines[row]?.getD ""
  let visCol := cur.col - props.scrollCol
  let visLen := min width (lineDisplayLength line - props.scrollCol)
  if visCol < visLen || visCol >= width then
    return lineToDraw
  let padCount := visCol - visLen
  return lineToDraw ++ ("".pushn ' ' padCount) ++ cur.spaceStyle ++ " " ++ props.resetStyle

end Bliku.Widget
