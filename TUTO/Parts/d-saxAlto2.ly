#(define part 'alt2)
\version "2.19.83"
\include "../NOTES.ily"

\header { instrument = "Saxo alto 2" }

\paper { 
  min-systems-per-page = 12
}

#(let ((m (em (voice 1 cl1) 10 11)))
(add-voice1 part 10  (adef m "(clar solo)" UP 1.2))
(rm part 3 (txt (markup #:fontsize adef-size "(clar solo)") UP -0.2))
(rm part 45 #{ \override Score.DynamicTextSpanner.minimum-length = #11 #}) ; for \cresc
(x-rm part break 11 C D 42)
)

\new Staff \transpose es c' { $(instru->music part) }