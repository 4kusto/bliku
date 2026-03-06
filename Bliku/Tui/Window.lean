import Bliku.Tui.Primitives

namespace Bliku.Tui

structure Cursor where
  row : Nat
  col : Nat
  deriving Repr, BEq, Inhabited

structure ViewState where
  bufferId : Nat
  cursor : Cursor
  scrollRow : Nat
  scrollCol : Nat
  deriving Repr, Inhabited

inductive Layout where
  | window (id : Nat) (view : ViewState)
  | hsplit (left right : Layout) (ratio : Float)
  | vsplit (top bottom : Layout) (ratio : Float)
  deriving Repr, Inhabited

structure FloatingWindowBorderFlags where
  top : Bool := true
  right : Bool := true
  bottom : Bool := true
  left : Bool := true
  deriving Inhabited

structure WorkspaceView where
  name : String
  layout : Layout
  activeWindowId : Nat
  floatingWindows : List Nat := []
  floatingWindowOffsets : List (Nat × Int × Int) := []
  deriving Inhabited

def Layout.findView (l : Layout) (id : Nat) : Option ViewState :=
  match l with
  | .window wid v => if wid == id then some v else none
  | .hsplit left right _ => (left.findView id).orElse (fun _ => right.findView id)
  | .vsplit top bottom _ => (top.findView id).orElse (fun _ => bottom.findView id)

def Layout.updateView (l : Layout) (id : Nat) (f : ViewState → ViewState) : Layout :=
  let rec go (node : Layout) : Layout × Bool :=
    match node with
    | .window wid v =>
      if wid == id then (.window wid (f v), true) else (node, false)
    | .hsplit left right ratio =>
      let (left', foundLeft) := go left
      if foundLeft then
        (.hsplit left' right ratio, true)
      else
        let (right', foundRight) := go right
        (.hsplit left' right' ratio, foundRight)
    | .vsplit top bottom ratio =>
      let (top', foundTop) := go top
      if foundTop then
        (.vsplit top' bottom ratio, true)
      else
        let (bottom', foundBottom) := go bottom
        (.vsplit top' bottom' ratio, foundBottom)
  (go l).1

def Layout.containsWindow (l : Layout) (windowId : Nat) : Bool :=
  match l with
  | .window wid _ => wid == windowId
  | .hsplit left right _ => left.containsWindow windowId || right.containsWindow windowId
  | .vsplit top bottom _ => top.containsWindow windowId || bottom.containsWindow windowId

def Layout.remove (l : Layout) (id : Nat) : Option Layout :=
  match l with
  | .window wid _ => if wid == id then none else some l
  | .hsplit left right ratio =>
    match left.remove id with
    | none => some right
    | some left' =>
      match right.remove id with
      | none => some left'
      | some right' => some (.hsplit left' right' ratio)
  | .vsplit top bottom ratio =>
    match top.remove id with
    | none => some bottom
    | some top' =>
      match bottom.remove id with
      | none => some top'
      | some bottom' => some (.vsplit top' bottom' ratio)

def getWindowIds (l : Layout) : List Nat :=
  match l with
  | .window id _ => [id]
  | .hsplit left right _ => getWindowIds left ++ getWindowIds right
  | .vsplit top bottom _ => getWindowIds top ++ getWindowIds bottom

def getAllWindowBounds (l : Layout) (h w : Nat) : List (Nat × Nat × Nat × Nat × Nat) :=
  let rec traverse (node : Layout) (r c h w : Nat) (acc : List (Nat × Nat × Nat × Nat × Nat)) :=
    match node with
    | .window id _ => (id, r, c, h, w) :: acc
    | .hsplit left right ratio =>
      let leftW := (Float.ofNat w * ratio).toUInt64.toNat
      let acc' := traverse left r c h leftW acc
      traverse right r (c + leftW + 1) h (if w > leftW then w - leftW - 1 else 0) acc'
    | .vsplit top bottom ratio =>
      let topH := (Float.ofNat h * ratio).toUInt64.toNat
      let acc' := traverse top r c topH w acc
      traverse bottom (r + topH + 1) c (if h > topH then h - topH - 1 else 0) w acc'
  traverse l 0 0 h w []

def WorkspaceView.isFloatingWindow (ws : WorkspaceView) (windowId : Nat) : Bool :=
  ws.floatingWindows.contains windowId

def WorkspaceView.getFloatingWindowIds (ws : WorkspaceView) : Array Nat :=
  ws.floatingWindows.foldl (fun acc wid =>
    match ws.layout.findView wid with
    | some _ => acc.push wid
    | none => acc) #[]

def WorkspaceView.floatingWindowIndex? (ws : WorkspaceView) (windowId : Nat) : Option Nat :=
  let ids := ws.getFloatingWindowIds
  let rec loop (i : Nat) : Option Nat :=
    if i >= ids.size then none
    else if ids[i]! == windowId then some i
    else loop (i + 1)
  loop 0

def WorkspaceView.getFloatingWindowOffset (ws : WorkspaceView) (windowId : Nat) : Int × Int :=
  match ws.floatingWindowOffsets.find? (fun (wid, _, _) => wid == windowId) with
  | some (_, ro, co) => (ro, co)
  | none => (0, 0)

def computeFloatingWindowBounds (rows cols idx : Nat) : Option (Nat × Nat × Nat × Nat) :=
  let availableRows := if rows > 1 then rows - 1 else rows
  if availableRows < 6 || cols < 16 then
    none
  else
    let hMax := availableRows - 2
    let wMax := cols - 4
    let h := min hMax (max 8 ((availableRows * 3) / 5))
    let w := min wMax (max 24 ((cols * 3) / 5))
    let topBase := (availableRows - h) / 2
    let leftBase := (cols - w) / 2
    let top := min (availableRows - h) (topBase + idx)
    let left := min (cols - w) (leftBase + (2 * idx))
    some (top, left, h, w)

def getFloatingWindowBounds (ws : WorkspaceView) (windowHeight windowWidth : Nat) (windowId : Nat) : Option (Nat × Nat × Nat × Nat) :=
  match ws.floatingWindowIndex? windowId with
  | none => none
  | some idx =>
    match computeFloatingWindowBounds windowHeight windowWidth idx with
    | none => none
    | some (top, left, h, w) =>
      let (rowOff, colOff) := ws.getFloatingWindowOffset windowId
      let toNatNonNeg (v : Int) := if v < 0 then 0 else Int.toNat v
      let availableRows := if windowHeight > 1 then windowHeight - 1 else windowHeight
      let maxTop := if availableRows > h then availableRows - h else 0
      let maxLeft := if windowWidth > w then windowWidth - w else 0
      let rawTop := Int.ofNat top + rowOff
      let rawLeft := Int.ofNat left + colOff
      let top' := min maxTop (toNatNonNeg rawTop)
      let left' := min maxLeft (toNatNonNeg rawLeft)
      some (top', left', h, w)

def getFloatingWindowBorderFlags (_ws : WorkspaceView) (_windowId : Nat) : FloatingWindowBorderFlags :=
  { top := true, right := true, bottom := true, left := true }

end Bliku.Tui
