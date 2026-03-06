namespace Bliku.Widget

structure PromptProps where
  leader : String := ""
  text : String := ""
  cursorCol : Nat := 0
  deriving Repr, Inhabited

def renderPrompt (props : PromptProps) : String :=
  props.leader ++ props.text

def promptCursorCol (props : PromptProps) : Nat :=
  props.leader.length + props.cursorCol

end Bliku.Widget
