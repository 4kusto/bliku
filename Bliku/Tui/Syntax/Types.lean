import Bliku.Style
import Bliku.Tui.Search

namespace Bliku.Tui.Syntax

inductive Language where
  | plain
  | lean
  | markdown
  deriving Repr, BEq, Inhabited

abbrev Color := Bliku.Color
abbrev Face := Bliku.Face

inductive HighlightKind where
  | keyword
  | comment
  | stringLiteral
  | numberLiteral
  | heading
  | code
  | link
  | emphasis
  | custom (name : String)
  deriving Repr, BEq, Inhabited

structure LanguageProfile where
  language : Language
  supportedKinds : Array HighlightKind := #[]
  deriving Repr, BEq, Inhabited

structure Span where
  startByte : Nat
  endByte : Nat
  kind : HighlightKind
  deriving Repr, BEq, Inhabited

structure Palette where
  keyword : Option Face := none
  comment : Option Face := none
  stringLiteral : Option Face := none
  numberLiteral : Option Face := none
  heading : Option Face := none
  code : Option Face := none
  link : Option Face := none
  emphasis : Option Face := none
  custom : List (String × Face) := []
  deriving Repr, BEq, Inhabited

private def mergeFace (base : Option Face) (override : Option Face) : Option Face :=
  match base, override with
  | none, none => none
  | some face, none => some face
  | none, some face => some face
  | some baseFace, some overrideFace =>
      some {
        fg := overrideFace.fg.orElse (fun _ => baseFace.fg)
        bg := overrideFace.bg.orElse (fun _ => baseFace.bg)
        bold := baseFace.bold || overrideFace.bold
        italic := baseFace.italic || overrideFace.italic
        underline := baseFace.underline || overrideFace.underline
      }

def Palette.merge (base overrides : Palette) : Palette :=
  {
    keyword := mergeFace base.keyword overrides.keyword
    comment := mergeFace base.comment overrides.comment
    stringLiteral := mergeFace base.stringLiteral overrides.stringLiteral
    numberLiteral := mergeFace base.numberLiteral overrides.numberLiteral
    heading := mergeFace base.heading overrides.heading
    code := mergeFace base.code overrides.code
    link := mergeFace base.link overrides.link
    emphasis := mergeFace base.emphasis overrides.emphasis
    custom := overrides.custom ++ base.custom
  }

def defaultPalette : Palette :=
  {
    keyword := some { fg := some (.ansi 12) }
    comment := some { fg := some (.ansi 8) }
    stringLiteral := some { fg := some (.ansi 10) }
    numberLiteral := some { fg := some (.ansi 13) }
    heading := some { fg := some (.ansi 14) }
    code := some { fg := some (.ansi 11) }
    link := some { fg := some (.ansi 12) }
    emphasis := some { fg := some (.ansi 13) }
  }

def Palette.faceFor (palette : Palette) : HighlightKind → Option Face
  | .keyword => palette.keyword
  | .comment => palette.comment
  | .stringLiteral => palette.stringLiteral
  | .numberLiteral => palette.numberLiteral
  | .heading => palette.heading
  | .code => palette.code
  | .link => palette.link
  | .emphasis => palette.emphasis
  | .custom name => palette.custom.find? (fun (n, _) => n == name) |>.map Prod.snd

def profileFor : Language → LanguageProfile
  | .plain => { language := .plain }
  | .lean => {
      language := .lean
      supportedKinds := #[.keyword, .comment, .stringLiteral, .numberLiteral]
    }
  | .markdown => {
      language := .markdown
      supportedKinds := #[.heading, .code, .link, .emphasis]
    }

def detectLanguage (filename : Option String) : Language :=
  match filename with
  | none => .plain
  | some name =>
      if name.endsWith ".lean" then .lean
      else if name.endsWith ".md" || name.endsWith ".markdown" then .markdown
      else .plain

def faceForByteRange (palette : Palette) (spans : Array Span) (byteStart byteEnd : Nat) : Option Face :=
  let rec loop (i : Nat) : Option Face :=
    if i >= spans.size then
      none
    else
      let s := spans[i]!
      if Bliku.Tui.overlapsByteRange (s.startByte, s.endByte) byteStart byteEnd then
        palette.faceFor s.kind
      else
        loop (i + 1)
  loop 0

end Bliku.Tui.Syntax
