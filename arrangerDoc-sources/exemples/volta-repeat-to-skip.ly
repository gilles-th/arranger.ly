%% volta-repeat-to-skip.ly
\version "2.24.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\layout {
  indent = 0
  \context { \Score
    \override BarNumber.break-visibility = ##(#t #t #t)    
    skipBars = ##t
    \override MultiMeasureRest.expand-limit = #1
  }
}

global = s1*100
#(init '())
#(begin
(signatures 1 "4/4" 24 "7/8" 33 "3/4") 
(cut-end 'global 50)
(rm 'global 50 #{ \bar "|." #})
)
#(init '(test))

#(begin
(def! 'structure)                
(rm-with 'structure             
    5 (volta-repeat->skip 9 3 1)                        
   29 (volta-repeat->skip (pos-sub 38 29) (* 2 3/4) 3/4)) 
(def! 'global (sim global structure)))


\score { 
  \new Staff << \global \test >>
}
