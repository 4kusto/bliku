namespace Bliku.Widget

structure ListItem where
  label : String
  deriving Repr, Inhabited

structure ListProps where
  items : Array ListItem
  selected : Nat := 0
  maxVisible : Nat := 8
  width : Nat := 0
  borderLeft : String := "|"
  borderRight : String := "|"
  borderFill : Char := '-'
  selectedStyle : String := "\x1b[7m"
  resetStyle : String := "\x1b[0m"
  deriving Inhabited

structure ListBox where
  lines : Array String
  width : Nat
  height : Nat
  deriving Inhabited

def renderListBox (props : ListProps) : ListBox := Id.run do
  if props.items.isEmpty then
    return { lines := #[], width := 0, height := 0 }
  let visibleCount := min props.maxVisible props.items.size
  let labels := props.items.extract 0 visibleCount |>.map ListItem.label
  let naturalW := labels.foldl (fun acc s => max acc s.length) 0
  let innerWidth := max 1 (if props.width == 0 then naturalW else props.width)
  let border := props.borderLeft ++ "".pushn props.borderFill innerWidth ++ props.borderRight
  let selected := if props.selected < visibleCount then props.selected else 0
  let mut out : Array String := #[border]
  for i in [0:visibleCount] do
    let label := (labels[i]!.take innerWidth).toString
    let pad := if label.length < innerWidth then "".pushn ' ' (innerWidth - label.length) else ""
    let body :=
      if i == selected then
        props.selectedStyle ++ label ++ pad ++ props.resetStyle
      else
        label ++ pad
    out := out.push (props.borderLeft ++ body ++ props.borderRight)
  out := out.push border
  return { lines := out, width := innerWidth + props.borderLeft.length + props.borderRight.length, height := out.size }

end Bliku.Widget
