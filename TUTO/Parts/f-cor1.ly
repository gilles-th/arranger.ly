#(define part 'cor1)
\version "2.19.83"
\include "../NOTES.ily"

\header { instrument = "Cor 1" }

\paper {
  min-systems-per-page = #9
}

dynOffset = \override Score.DynamicText.extra-offset = \etc
revertOffset = \revert Score.DynamicText.extra-offset
hairpinOffset = \override Score.Hairpin.extra-offset = \etc
revertHairpinOffset = \revert Score.Hairpin.extra-offset

#(let ((m (em cl1 17 19)))
(add-voice2 part 17  (adef m (markup #:fontsize (- adef-size) "(clar-tpttes)") DOWN -0.5))
(rm-with part
   3 (txt "(clar solo)" UP CENTER)
  11 (txt "(piccolo)" UP -0.2))
(merge-in part 41 #{
   \dynOffset #'(0.5 . 0) s8
   \hairpinOffset #'(0 . -2) s4.
   \hairpinOffset #'(0 . -1) s2
   \revertOffset \revertHairpinOffset #})
(x-rm part break C 27 D 39 42))

\new Staff \transpose f c' { $(instru->music part) }

\markup { \vspace  #5 }
