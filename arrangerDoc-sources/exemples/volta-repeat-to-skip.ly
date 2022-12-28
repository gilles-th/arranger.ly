%% volta-repeat-to-skip.ly
\version "2.24.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\layout {
  indent = 0
  \context { 
    \Score
    \override BarNumber.break-visibility = #all-visible
    \override BarNumber.self-alignment-X = #CENTER   
    skipBars = ##t
    \override MultiMeasureRest.expand-limit = #1
  }
}

global = s1*100
#(init '())
#(begin
(signatures 1 "4/4" 24 "7/8" 33 "3/4")
(cut-end 'global 71)
(rm 'global 71 #{ \bar "|." #})
)
#(init '(test))

#(begin
(def! 'structure)
(rm-with 'structure
    5 (volta-repeat->skip 9 3 1)
   29 (seq break (volta-repeat->skip (pos-sub 38 29) (* 2 3/4) 3/4))
   50 (volta-repeat->skip (* 9 3/4) '((3/4 1 2) (3/4 3 4 5) (3/4 6 7)))
  )
(def! 'global (sim global structure)))


\score {
  \new Staff << \global \test >>
}

