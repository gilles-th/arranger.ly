%% example 5
\version "2.24.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\layout { \context { \Score
  \override BarNumber.break-visibility = #all-visible
  skipBars = ##t
  \override BarNumber.font-size = #+2
  \override MultiMeasureRest.expand-limit = #1 }}


global = { s1*3 \bar "|." }
instrus = #'(I II III)
#(init instrus)

chords = \relative c' { <b f' gis> <d f b> <c e a> <b d e> }

#(begin
(fill instrus (list #{ r8 e'-. #}
                    #{ r8 c'-. #}
                    #{ a8-> r c'-. r b-. r a-. r #})
              1 4)
(apply-to instrus (cons set-pitch (chords->nmusics 3 chords))
                  2 3)
)

\score {\new StaffGroup <<
  \new Staff \with {instrumentName = "I"} << \global \I >>
  \new Staff \with {instrumentName = "II"} << \global \II >>
  \new Staff \with {instrumentName = "III"} << \global \III >>
>>}
