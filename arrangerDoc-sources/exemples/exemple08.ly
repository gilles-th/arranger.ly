%% exemple08.ly
\version "2.24.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\paper { ragged-right = ##t }

\layout { \context { 
  \Score
  \override BarNumber.break-visibility = ##(#f #t #f)
  \override BarNumber.self-alignment-X = #CENTER
  skipBars = ##t
  \override BarNumber.font-size = #+2
  \override MultiMeasureRest.expand-limit = #1
  }
}
global = {s1*12 \bar "|."}
cls = #'(clI clII)
#(init cls)

music = { e'2 f' | g' f' | e'1 }
#(begin
  (rm cls 10 music)
  (apply x-rm 'clII #{ c'8 c' c' #} (x-pos 10 13 '((n 8)(n 2 8))))
)

\new StaffGroup <<
  \new Staff \with{ instrumentName = "I" } { << \global \clI >> }
  \new Staff \with{ instrumentName = "II" } { << \global \clII >> }
  >> 
