#(define part 'ctb)
\version "2.19.83"
\include "../NOTES.ily"

\header { instrument = "Contrebasse à cordes" }

\paper { 
  min-systems-per-page = 10
}

#(let ((m (seq (make-cue-clef-set "treble")  ; defined in scm/parser-clef.scm
               (em (voice 1 cl1) 10 11)
               (make-cue-clef-unset))))
(add-voice1 part 10  (adef m "(clar solo)" UP 1.2))
(rm part 3 (txt (markup #:fontsize adef-size "(clar solo)") UP -0.2))
(x-rm part break 11 C D 42)
)

\markup { \vspace  #2 }

\new Staff \transpose c c' { $(instru->music part "bass") }

\markup { \vspace  #12 }
