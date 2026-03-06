import Bliku.Tui.Search

namespace Bliku.Tui.Syntax

inductive Language where
  | plain
  | lean
  | markdown
  deriving Repr, BEq, Inhabited

structure Span where
  startByte : Nat
  endByte : Nat
  style : String
  deriving Repr, BEq, Inhabited

-- ANSI bright colors.
def leanKeywordStyle : String := "\x1b[94m"
def leanCommentStyle : String := "\x1b[90m"
def leanStringStyle : String := "\x1b[92m"
def leanNumberStyle : String := "\x1b[95m"

def markdownHeadingStyle : String := "\x1b[96m"
def markdownCodeStyle : String := "\x1b[93m"
def markdownLinkStyle : String := "\x1b[94m"
def markdownEmphasisStyle : String := "\x1b[95m"

def detectLanguage (filename : Option String) : Language :=
  match filename with
  | none => .plain
  | some name =>
      if name.endsWith ".lean" then .lean
      else if name.endsWith ".md" || name.endsWith ".markdown" then .markdown
      else .plain

def styleForByteRange (spans : Array Span) (byteStart byteEnd : Nat) : Option String :=
  let rec loop (i : Nat) : Option String :=
    if i >= spans.size then
      none
    else
      let s := spans[i]!
      if Bliku.Tui.overlapsByteRange (s.startByte, s.endByte) byteStart byteEnd then
        some s.style
      else
        loop (i + 1)
  loop 0

end Bliku.Tui.Syntax
