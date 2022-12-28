%% example 3
\version "2.24.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\layout { \context {       
  \Score
  \override BarNumber.break-visibility = #all-visible
  \override BarNumber.self-alignment-X = #CENTER
  \override BarNumber.font-size = #+2
  skipBars = ##t
  \override MultiMeasureRest.expand-limit = #1 }
}

global = { s1*8 \bar "|." }
all = #'(flute clar basse)
#(init all)

mus = \relative c' { f,1 c' f a f' } % clar mes 4

#(begin
(rm 'flute 7 mus 4)
(rm 'clar 4 mus #f)
(rm 'basse 4 mus 6))

\score {\new StaffGroup <<
  \new Staff \with {instrumentName = "fl"} << \global \flute >>
  \new Staff \with {instrumentName = "cl"} << \global \clar >>
  \new Staff \with {instrumentName = "bs"} << \clef F \global \basse >>
>>}
