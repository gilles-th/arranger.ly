#(define part 'cor2)
\version "2.19.83"
\include "../NOTES.ily"

\paper {
  min-systems-per-page = #9
}

\header { instrument = "Cor 2" }

#(let ((m (em cl1 17 19)))
(add-voice2 part 17  (adef m (markup #:fontsize (- adef-size) "(clar-tpttes)") DOWN -0.5))
(rm-with part
   3 (txt "(clar solo)" UP CENTER)
  11 (txt "(piccolo)" UP -0.2)
  41 #{ \once \override Score.TextScript.direction = #UP #})
(x-rm part break C 27 D 39 42))

\new Staff \transpose f c' { $(instru->music part) }

\markup { \vspace  #5 }
