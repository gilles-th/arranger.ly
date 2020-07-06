#(define part 'tuba)
\version "2.19.83"
\include "../NOTES.ily"

\header { instrument = "Tuba ut" }

\paper {
  min-systems-per-page = 8
}

#(let ((m (seq (make-cue-clef-set "treble")  ; defined in scm/parser-clef.scm
               (em cl1 17 19)
               (make-cue-clef-unset))))
(add-voice2 part 17  (adef m (markup #:fontsize (- adef-size) "(clar-tpttes)") DOWN 1.2))
(rm-with part
   3 (txt "(clar solo)" UP CENTER)
  11 (txt "(piccolo)" UP -0.2))

(x-rm part break C 27 D 42))

\markup { \vspace  #2 }

\new Staff { $(instru->music part "bass") }

\markup { \vspace  #10 }
