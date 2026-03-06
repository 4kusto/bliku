import Bliku.Style

namespace Bliku

structure ByteDecoration where
  row : Nat
  startByte : Nat
  endByte : Nat
  priority : Nat := 0
  style : String
  deriving Repr, BEq, Inhabited

structure CellDecoration where
  row : Nat
  startCol : Nat
  endCol : Nat
  priority : Nat := 0
  style : String
  deriving Repr, BEq, Inhabited

structure CursorDecoration where
  row : Nat
  col : Nat
  charStyle : String
  spaceStyle : String
  priority : Nat := 1000
  deriving Repr, BEq, Inhabited

end Bliku
