import Bliku.Tui.Syntax.Types
import Bliku.Tui.Syntax.Lean
import Bliku.Tui.Syntax.Markdown

namespace Bliku.Tui.Syntax

def highlightLine (filename : Option String) (line : String) : Array Span :=
  match detectLanguage filename with
  | .plain => #[]
  | .lean => highlightLeanLine line
  | .markdown => highlightMarkdownLine line

end Bliku.Tui.Syntax
