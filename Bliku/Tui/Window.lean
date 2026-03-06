import Bliku.Layout

namespace Bliku.Tui

abbrev Cursor := Bliku.Cursor
abbrev ViewState := Bliku.PaneView
abbrev Layout := Bliku.PaneLayout

def ViewState.bufferId (view : ViewState) : Nat :=
  view.contentId

structure FloatingWindowBorderFlags where
  top : Bool := true
  right : Bool := true
  bottom : Bool := true
  left : Bool := true
  deriving Inhabited

structure WorkspaceView where
  name : String
  desktop : Bliku.DesktopLayout
  deriving Inhabited

def WorkspaceView.layout (ws : WorkspaceView) : Layout :=
  ws.desktop.layout

def WorkspaceView.activeWindowId (ws : WorkspaceView) : Nat :=
  ws.desktop.activePaneId

def WorkspaceView.floatingWindows (ws : WorkspaceView) : List Nat :=
  (ws.desktop.floating.getClusters ws.layout).foldl
    (fun acc cluster =>
      match ws.layout.extractFloatingRoot cluster.root with
      | some subtree => acc ++ subtree.getPaneIds
      | none => acc)
    []

def WorkspaceView.floatingWindowOffsets (ws : WorkspaceView) : List (Nat × Int × Int) :=
  (ws.desktop.floating.getClusters ws.layout).foldl
    (fun acc cluster =>
      match cluster.root with
      | .pane wid => (wid, cluster.rowOffset, cluster.colOffset) :: acc
      | .group _ => acc)
    []

def WorkspaceView.floatingClusters (ws : WorkspaceView) : Array Bliku.FloatingCluster :=
  ws.desktop.floating.getClusters ws.layout

def Layout.findView (l : Layout) (id : Nat) : Option ViewState :=
  Bliku.PaneLayout.findView l id

def Layout.updateView (l : Layout) (id : Nat) (f : ViewState → ViewState) : Layout :=
  Bliku.PaneLayout.updateView l id f

def Layout.containsWindow (l : Layout) (windowId : Nat) : Bool :=
  l.getPaneIds.contains windowId

def Layout.remove (l : Layout) (id : Nat) : Option Layout :=
  Bliku.PaneLayout.remove l id

def getWindowIds (l : Layout) : List Nat :=
  l.getPaneIds

def getAllWindowBounds (l : Layout) (h w : Nat) : List (Nat × Nat × Nat × Nat × Nat) :=
  l.getAllPaneBounds h w

def WorkspaceView.isFloatingWindow (ws : WorkspaceView) (windowId : Nat) : Bool :=
  ws.desktop.floating.clusterForPane? ws.layout windowId |>.isSome

def WorkspaceView.getFloatingWindowIds (ws : WorkspaceView) : Array Nat :=
  ws.floatingClusters.foldl (fun acc cluster =>
    match ws.layout.extractFloatingRoot cluster.root with
    | some subtree => acc ++ subtree.getPaneIds.toArray
    | none => acc) #[]

def WorkspaceView.floatingWindowIndex? (ws : WorkspaceView) (windowId : Nat) : Option Nat :=
  let clusters := ws.floatingClusters
  let rec loop (i : Nat) : Option Nat :=
    if i >= clusters.size then
      none
    else
      match ws.layout.extractFloatingRoot clusters[i]!.root with
      | some subtree =>
          if subtree.getPaneIds.contains windowId then some i else loop (i + 1)
      | none => loop (i + 1)
  loop 0

def WorkspaceView.getFloatingWindowOffset (ws : WorkspaceView) (windowId : Nat) : Int × Int :=
  match ws.desktop.floating.clusterForPane? ws.layout windowId with
  | some cluster => (cluster.rowOffset, cluster.colOffset)
  | none => (0, 0)

def computeFloatingWindowBounds (rows cols idx : Nat) : Option (Nat × Nat × Nat × Nat) :=
  Bliku.computeFloatingClusterBounds rows cols idx { root := .pane idx }

def computeFloatingClusterBounds (rows cols idx : Nat) (cluster : Bliku.FloatingCluster) : Option (Nat × Nat × Nat × Nat) :=
  Bliku.computeFloatingClusterBounds rows cols idx cluster

def getFloatingWindowBounds (ws : WorkspaceView) (windowHeight windowWidth : Nat) (windowId : Nat) : Option (Nat × Nat × Nat × Nat) :=
  match ws.floatingWindowIndex? windowId with
  | none => none
  | some idx =>
    match ws.desktop.floating.clusterForPane? ws.layout windowId with
    | none => none
    | some cluster =>
      match computeFloatingClusterBounds windowHeight windowWidth idx cluster with
      | none => none
      | some (top, left, h, w) =>
        let (rowOff, colOff) := (cluster.rowOffset, cluster.colOffset)
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
