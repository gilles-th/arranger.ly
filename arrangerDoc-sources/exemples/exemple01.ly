%% exemple1.ly
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

global = {
 \partial 4 s4
  s1*2
  % measure 3 : only 2 beats
  s4 \set Timing.measurePosition = #(ly:make-moment 3/4)
     s4
  s1 % measure 4
  \set Score.currentBarNumber = #50
  %\set Timing.currentBarNumber = #50
  s1  % measure 50 !
  \bar "|."
}
all = #'(flute clar sax tptte cor tbne basse)
#(init all)

#(display (map measure-number->moment '(1 2 3 4 50)))
% => (#<Mom 1/4> #<Mom 5/4> #<Mom 9/4> #<Mom 11/4> #<Mom 15/4>)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\score {
  \new StaffGroup <<
    \new Staff \with {instrumentName = "fl"} << \global \flute >>
    \new Staff \with {instrumentName = "cl"} << \global \clar >>
    \new Staff \with {instrumentName = "sax"} << \global \sax >>
    \new Staff \with {instrumentName = "tptte"} << \global \tptte >>
    \new Staff \with {instrumentName = "cor"} << \global \cor >>
    \new Staff \with {instrumentName = "tbne"} << \global \clef F \tbne >>
    \new Staff \with {instrumentName = "basse"} << \global \clef F \basse >>
>>}
