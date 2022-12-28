%% example 4
\version "2.24.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\layout { \context { \Score
  \override BarNumber.break-visibility = ##(#f #t #f)
  \override BarNumber.self-alignment-X = #CENTER
  }
}

cadenza = \relative c' { c4^"cadenza" d e f g }

global = {
  \time 3/4
  s2.
  \cadenzaOn
  #(skip-of-length cadenza)
  \cadenzaOff
  s2.*2
  \bar "|."
}

#(init '(clar))
#(rm 'clar 2 #{ \cadenza r2. #})
