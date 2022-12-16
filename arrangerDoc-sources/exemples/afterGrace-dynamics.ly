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

global = s1*2
all = #'(instru1 instru2)
#(init all)

% default fraction AfterGraceFraction = 3/4
musicI = { r2 r4 \afterGrace d4-> { es16( e f fis } g4->) r r2 }
% custom fraction (15/16)
musicII = { r2 r4 \afterGrace 15/16 d4-> { es16( e f fis } g4->) r r2 }

#(rm all 1 (rel 1 musicI musicII))

#(begin
(def! '(dyn1 dyn2))
; default = afterGraceFraction (3/4)
(add-dynamics 'dyn1 "(1 2.) f::4 p:16 <:16 :8 / 2 f")
; custom fraction (15/16)
(add-dynamics 'dyn2 "(1 2.) f::4:15:16 p:16 <:16 :8 / 2 f")
)

markI = <>^"fraction = afterGraceFraction = 3/4"
markII = <>^"fraction = 15/16"

\score { <<
  \new Staff $(sim global markI instru1 dyn1)
  \new Staff $(sim global markII instru2 dyn2)
>>
}
