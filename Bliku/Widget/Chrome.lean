import Bliku.Layout

namespace Bliku.Widget

structure ChromeBox where
  lines : Array String
  insetTop : Nat := 0
  insetLeft : Nat := 0
  insetBottom : Nat := 0
  insetRight : Nat := 0
  deriving Inhabited

def renderFloatingChromeBox
    (chrome : Bliku.FloatingChrome)
    (width height : Nat)
    (activeStyle inactiveStyle resetStyle : String)
    (isActive : Bool) : ChromeBox := Id.run do
  if chrome.kind == .none || width < 2 || height < 2 then
    return { lines := #[] }
  let style := if isActive then activeStyle else inactiveStyle
  let title := chrome.title.getD ""
  let topFill := if width > 2 then width - 2 else 0
  let rawTop := if title.isEmpty then "".pushn '─' topFill else
    let core := s!" {title} "
    let clipped := (core.take topFill).toString
    clipped ++ "".pushn '─' (topFill - clipped.length)
  let top := style ++ "┌" ++ rawTop ++ "┐" ++ resetStyle
  let middle := style ++ "│" ++ resetStyle ++ "".pushn ' ' (width - 2) ++ style ++ "│" ++ resetStyle
  let bottom := style ++ "└" ++ "".pushn '─' topFill ++ "┘" ++ resetStyle
  let mut lines : Array String := #[top]
  for _ in [0:height - 2] do
    lines := lines.push middle
  lines := lines.push bottom
  return {
    lines := lines
    insetTop := 1
    insetLeft := 1
    insetBottom := 1
    insetRight := 1
  }

end Bliku.Widget
