%% grace-dynamics.ly
\version "2.24.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\layout {
  indent = 0
  \context { \Score   
    skipBars = ##t
    \override MultiMeasureRest.expand-limit = #1
  }
}

global = s1
all = #'(instru1 instru2 instru3)
#(init all)

music = { d2 \acciaccatura { c16 cis d } es2 }

#(rm all 1 (rel 1 0 -1 music))

#(begin
(def! '(dyn1 dyn2 dyn3))   
(add-dynamics 'dyn1 ; basic cresc
    "1 p / (1 2) <:16 :16*2 f")      
(add-dynamics 'dyn2 ; no tweaks
    "1 p / (1 2) :16 mp:16 mf:16 f") 
(add-dynamics 'dyn3 ; with tweaks
    "1 p / (1 2) :16 mp:16#1.3#-1.2 mf:16#0#-0.6 f#-0.2#-0.6") 
)

\score {
   <<
     \new Staff $(sim global instru1 dyn1)
     \new Staff $(sim global instru2 dyn2)
     \new Staff $(sim global instru3 (make-clef-set "bass") dyn3)
   >>
}
