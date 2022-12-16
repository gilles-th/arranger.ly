%% fill-percent.ly
\version "2.24.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\layout {
  \context {
    \Score \override BarNumber.break-visibility = #all-visible
    \override BarNumber.font-size = #+2
  }
}

global = { s1*4 \bar "|." }
all = #'(I II)
#(init all)

#(begin
(fill-percent 'I #{ c'4 d' e' f' #} 1 4)
(fill-percent 'II #{ c'4 d' #} 1 4)
)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\score {
  \new StaffGroup <<
    \new Staff \with {instrumentName = \markup { \hspace #6 "I"}} << \global \I >>
    \new Staff \with {instrumentName = \markup { \hspace #6 "II"}} << \global \II >>
>>}
