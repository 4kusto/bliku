namespace Bliku.Widget

structure PopupProps where
  title : String := ""
  lines : Array String := #[]
  maxWidth : Nat := 0
  borderCorner : String := "+"
  borderHorizontal : Char := '-'
  borderVertical : String := "|"
  deriving Inhabited

structure PopupBox where
  lines : Array String
  innerWidth : Nat
  titleRows : Nat
  contentRows : Nat
  deriving Inhabited

def renderPopupBox (props : PopupProps) : PopupBox := Id.run do
  let lines := if props.lines.isEmpty then #[""] else props.lines
  let titleText := if props.title.isEmpty then "" else s!"[{props.title}]"
  let titleRows := if titleText.isEmpty then 0 else 1
  let naturalWidthContent := lines.foldl (fun acc ln => max acc ln.length) 0
  let naturalWidth := max naturalWidthContent titleText.length
  let innerWidth := max 1 (if props.maxWidth > 0 then min props.maxWidth naturalWidth else naturalWidth)
  let border := props.borderCorner ++ "".pushn props.borderHorizontal (innerWidth + 2) ++ props.borderCorner
  let mut out : Array String := #[border]
  if titleRows == 1 then
    let clipped := (titleText.take innerWidth).toString
    let pad := if clipped.length < innerWidth then "".pushn ' ' (innerWidth - clipped.length) else ""
    out := out.push s!"{props.borderVertical} {clipped}{pad} {props.borderVertical}"
  for raw in lines do
    let clipped := (raw.take innerWidth).toString
    let pad := if clipped.length < innerWidth then "".pushn ' ' (innerWidth - clipped.length) else ""
    out := out.push s!"{props.borderVertical} {clipped}{pad} {props.borderVertical}"
  out := out.push border
  return { lines := out, innerWidth := innerWidth, titleRows := titleRows, contentRows := lines.size }

end Bliku.Widget
