%% example 2
\version "2.20.0"

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
all = #'(fl cl sax tpt horn tbn bass)
#(init all)

musA = \relative c' { e2 d c1}
musB = { f1 e1} 
musC = { g,1 c1}

#(begin  (rm 'fl 1 #{ c'''1 #})
(rm '(cl sax tpt) 2 #{ c''1 #})
(rm '(horn tbn bass) 3 '(musA musB musC)))

\score {\new StaffGroup <<
 \new Staff \with {instrumentName = "fl"} << \global \fl >>
 \new Staff \with {instrumentName = "cl"} << \global \cl >>
 \new Staff \with {instrumentName = "sax"} << \global \sax >>
 \new Staff \with {instrumentName = "tpt"} << \global \tpt >>
 \new Staff \with {instrumentName = "horn"} << \global \horn >>
 \new Staff \with {instrumentName = "tbn"} << \global \clef F \tbn >>
 \new Staff \with {instrumentName = "bass"} << \global \clef F \bass >>
>>}
