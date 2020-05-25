%% example 2
\version "2.19.83"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\layout {
  \context {
    \Score \override BarNumber.break-visibility = #all-visible
    \override BarNumber.font-size = #+2
    barNumberVisibility = #all-bar-numbers-visible
  }
}

global = { s1*4 \bar "|." }
all = #'(flute clar sax tptte cor tbne basse)
#(init all)

musA = \relative c' { e2 d c1}
musB = { f1 e1} 
musC = { g,1 c1}

#(begin  (rm 'flute 1 #{ c'''1 #})
(rm '(clar sax tptte) 2 #{ c''1 #})
(rm '(cor tbne basse) 3 '(musA musB musC)))

\score {\new StaffGroup <<
 \new Staff \with {instrumentName = "fl"} << \global \flute >>
 \new Staff \with {instrumentName = "cl"} << \global \clar >>
 \new Staff \with {instrumentName = "sax"} << \global \sax >>
 \new Staff \with {instrumentName = "tptte"} << \global \tptte >>
 \new Staff \with {instrumentName = "cor"} << \global \cor >>
 \new Staff \with {instrumentName = "tbne"} << \global \clef F \tbne >>
 \new Staff \with {instrumentName = "basse"} << \global \clef F \basse >>
>>}
