%% example 4
\version "2.20.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\layout { \context { \Score
  \override BarNumber.break-visibility = #all-visible
  barNumberVisibility = #all-bar-numbers-visible
  skipBars = ##t
  \override BarNumber.font-size = #+2
  \override MultiMeasureRest.expand-limit = #1
  }
}

cadenza = \relative c' { c4^"cadenza" d e f g }

global = {
  \time 3/4
  s2.
  \cadenzaOn
  #(skip-of-length cadenza) \bar "|"
  \cadenzaOff
  s2.*2
  \bar "|."
}

#(init '(clar))
#(begin
(rm 'clar 2 cadenza)
(rm 'clar 3 #{ c'2. #})
;(rm 'clar '(3 -2 -4) #{ e'2. #})
;(rm 'clar `(2 ,(ly:music-length cadenza)) #{ e'2. #})
)



\new Staff {<< \global \clar >>}
