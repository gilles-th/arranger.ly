#(define part 'piccolo)
\version "2.24.0"
\include "../NOTES.ily"

\header { instrument = "Piccolo" }


\paper { 
  min-systems-per-page = 9
}

#(let ((m (octave +1 (em cl1 10 11))))
(add-voice1 part 10  (adef (octave +1 (voice 1 m))
                               "(clar solo (8° bassia))"))
(rm part 3 (txt (markup #:fontsize adef-size "(clar solo)") UP -0.2))
(x-rm part break 11 C)
)

\markup \vspace #1

\new Staff \transpose c' c { $(instru->music part) }

\markup \vspace #1
