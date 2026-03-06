namespace Bliku

structure Cursor where
  row : Nat
  col : Nat
  deriving Repr, BEq, Inhabited

structure PaneView where
  contentId : Nat
  cursor : Cursor
  scrollRow : Nat
  scrollCol : Nat
  deriving Repr, Inhabited

inductive FloatingRootRef where
  | pane (id : Nat)
  | group (id : Nat)
  deriving Repr, BEq, Inhabited

inductive FloatingAxisSize where
  | auto
  | fixed (value : Nat)
  | ratio (value : Float)
  deriving Repr, BEq, Inhabited

structure FloatingSizeSpec where
  width : FloatingAxisSize := .auto
  height : FloatingAxisSize := .auto
  minWidth : Option Nat := none
  minHeight : Option Nat := none
  maxWidth : Option Nat := none
  maxHeight : Option Nat := none
  deriving Repr, BEq, Inhabited

inductive FloatingSizePolicy where
  | default
  | multiPane
  | custom (spec : FloatingSizeSpec)
  deriving Repr, BEq, Inhabited

inductive FloatingChromeKind where
  | none
  | bordered
  deriving Repr, BEq, Inhabited

structure FloatingChrome where
  kind : FloatingChromeKind := .none
  title : Option String := none
  deriving Repr, BEq, Inhabited

inductive PaneLayout where
  | pane (id : Nat) (view : PaneView)
  | group (id : Nat) (body : PaneLayout)
  | hsplit (left right : PaneLayout) (ratio : Float)
  | vsplit (top bottom : PaneLayout) (ratio : Float)
  deriving Repr, Inhabited

structure FloatingCluster where
  root : FloatingRootRef
  rowOffset : Int := 0
  colOffset : Int := 0
  sizePolicy : FloatingSizePolicy := .default
  chrome : FloatingChrome := {}
  deriving Repr, BEq, Inhabited

structure FloatingPaneState where
  clusters : Array FloatingCluster := #[]
  deriving Inhabited

structure DesktopLayout where
  layout : PaneLayout
  activePaneId : Nat
  floating : FloatingPaneState := {}
  deriving Inhabited

def PaneLayout.findView (l : PaneLayout) (id : Nat) : Option PaneView :=
  match l with
  | .pane wid v => if wid == id then some v else none
  | .group _ body => body.findView id
  | .hsplit left right _ => (left.findView id).orElse (fun _ => right.findView id)
  | .vsplit top bottom _ => (top.findView id).orElse (fun _ => bottom.findView id)

def PaneLayout.updateView (l : PaneLayout) (id : Nat) (f : PaneView → PaneView) : PaneLayout :=
  let rec go (node : PaneLayout) : PaneLayout × Bool :=
    match node with
    | .pane wid v =>
      if wid == id then (.pane wid (f v), true) else (node, false)
    | .group gid body =>
      let (body', found) := go body
      (.group gid body', found)
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

def PaneLayout.remove (l : PaneLayout) (id : Nat) : Option PaneLayout :=
  match l with
  | .pane wid _ => if wid == id then none else some l
  | .group gid body =>
    if gid == id then
      none
    else
      body.remove id |>.map (PaneLayout.group gid)
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

def PaneLayout.getPaneIds (l : PaneLayout) : List Nat :=
  match l with
  | .pane id _ => [id]
  | .group _ body => body.getPaneIds
  | .hsplit left right _ => left.getPaneIds ++ right.getPaneIds
  | .vsplit top bottom _ => top.getPaneIds ++ bottom.getPaneIds

def PaneLayout.containsFloatingRoot (l : PaneLayout) (root : FloatingRootRef) : Bool :=
  match l, root with
  | .pane id _, .pane target => id == target
  | .group id _, .group target => id == target
  | .group _ body, root => body.containsFloatingRoot root
  | .hsplit left right _, root => left.containsFloatingRoot root || right.containsFloatingRoot root
  | .vsplit top bottom _, root => top.containsFloatingRoot root || bottom.containsFloatingRoot root
  | _, _ => false

def PaneLayout.extractFloatingRoot (l : PaneLayout) (root : FloatingRootRef) : Option PaneLayout :=
  match l, root with
  | .pane id _, .pane target => if id == target then some l else none
  | .group id _, .group target => if id == target then some l else none
  | .group _ body, root => body.extractFloatingRoot root
  | .hsplit left right _, root => (left.extractFloatingRoot root).orElse (fun _ => right.extractFloatingRoot root)
  | .vsplit top bottom _, root => (top.extractFloatingRoot root).orElse (fun _ => bottom.extractFloatingRoot root)
  | _, _ => none

def PaneLayout.removeFloatingRoot (l : PaneLayout) (root : FloatingRootRef) : Option PaneLayout :=
  match l, root with
  | .pane id _, .pane target => if id == target then none else some l
  | .group id _, .group target => if id == target then none else some l
  | .group gid body, root =>
    body.removeFloatingRoot root |>.map (PaneLayout.group gid)
  | .hsplit left right ratio, root =>
    match left.removeFloatingRoot root with
    | none => some right
    | some left' =>
      match right.removeFloatingRoot root with
      | none => some left'
      | some right' => some (.hsplit left' right' ratio)
  | .vsplit top bottom ratio, root =>
    match top.removeFloatingRoot root with
    | none => some bottom
    | some top' =>
      match bottom.removeFloatingRoot root with
      | none => some top'
      | some bottom' => some (.vsplit top' bottom' ratio)
  | _, _ => some l

def PaneLayout.getAllPaneBounds (l : PaneLayout) (h w : Nat) : List (Nat × Nat × Nat × Nat × Nat) :=
  let rec traverse (node : PaneLayout) (r c h w : Nat) (acc : List (Nat × Nat × Nat × Nat × Nat)) :=
    match node with
    | .pane id _ => (id, r, c, h, w) :: acc
    | .group _ body => traverse body r c h w acc
    | .hsplit left right ratio =>
      let leftW := (Float.ofNat w * ratio).toUInt64.toNat
      let acc' := traverse left r c h leftW acc
      traverse right r (c + leftW + 1) h (if w > leftW then w - leftW - 1 else 0) acc'
    | .vsplit top bottom ratio =>
      let topH := (Float.ofNat h * ratio).toUInt64.toNat
      let acc' := traverse top r c topH w acc
      traverse bottom (r + topH + 1) c (if h > topH then h - topH - 1 else 0) w acc'
  traverse l 0 0 h w []

private def clampSize (value fallback minValue maxValue : Nat) : Nat :=
  max minValue (min maxValue (if value == 0 then fallback else value))

private def resolveAxisSize (available fallback minValue maxValue : Nat) : FloatingAxisSize → Nat
  | .auto => clampSize fallback fallback minValue maxValue
  | .fixed value => clampSize value fallback minValue maxValue
  | .ratio ratio =>
      let value := (Float.ofNat available * ratio).toUInt64.toNat
      clampSize value fallback minValue maxValue

private def defaultFloatingSizeSpec : FloatingSizeSpec :=
  {
    width := .ratio 0.6
    height := .ratio 0.6
    minWidth := some 24
    minHeight := some 8
  }

private def multiPaneFloatingSizeSpec : FloatingSizeSpec :=
  {
    width := .ratio 0.8
    height := .ratio 0.7
    minWidth := some 60
    minHeight := some 16
  }

private def sizeSpecForPolicy : FloatingSizePolicy → FloatingSizeSpec
  | .default => defaultFloatingSizeSpec
  | .multiPane => multiPaneFloatingSizeSpec
  | .custom spec => spec

def computeFloatingClusterBounds (rows cols idx : Nat) (cluster : FloatingCluster) : Option (Nat × Nat × Nat × Nat) :=
  let availableRows := if rows > 1 then rows - 1 else rows
  if availableRows < 6 || cols < 16 then
    none
  else
    let spec := sizeSpecForPolicy cluster.sizePolicy
    let hMax := availableRows - 2
    let wMax := cols - 4
    let defaultH := max 8 ((availableRows * 3) / 5)
    let defaultW := max 24 ((cols * 3) / 5)
    let minH := spec.minHeight.getD 1
    let minW := spec.minWidth.getD 1
    let maxH := spec.maxHeight.map (min hMax) |>.getD hMax
    let maxW := spec.maxWidth.map (min wMax) |>.getD wMax
    let h := resolveAxisSize availableRows defaultH minH maxH spec.height
    let w := resolveAxisSize cols defaultW minW maxW spec.width
    let topBase := (availableRows - h) / 2
    let leftBase := (cols - w) / 2
    let top := min (availableRows - h) (topBase + idx)
    let left := min (cols - w) (leftBase + (2 * idx))
    some (top, left, h, w)

def FloatingPaneState.getClusters (layout : PaneLayout) (floating : FloatingPaneState) : Array FloatingCluster :=
  floating.clusters.foldl (fun acc cluster =>
    if layout.containsFloatingRoot cluster.root then acc.push cluster else acc) #[]

def FloatingPaneState.clusterForPane? (layout : PaneLayout) (floating : FloatingPaneState) (paneId : Nat) : Option FloatingCluster :=
  let clusters := floating.getClusters layout
  let rec loop (i : Nat) : Option FloatingCluster :=
    if i >= clusters.size then
      none
    else
      let cluster := clusters[i]!
      match layout.extractFloatingRoot cluster.root with
      | some subtree =>
          if subtree.getPaneIds.contains paneId then some cluster else loop (i + 1)
      | none => loop (i + 1)
  loop 0

end Bliku
