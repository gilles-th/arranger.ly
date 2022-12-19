#(define part 'tb2)
\version "2.24.0"
\include "../NOTES.ily"

\paper {
  min-systems-per-page = 6
  ragged-last-bottom = ##f
}

\header { instrument = "Trombone 2" }

#(let ((m (seq (make-cue-clef-set "treble")  ; defined in scm/parser-clef.scm
               (em tp1 25 27)
               (make-cue-clef-unset))))
(add-voice2 part 25 (adef m (markup #:fontsize (- adef-size) "(tpttes)") DOWN -0.5))
(rm-with part
   3 (txt "(clar solo)" UP CENTER)
  11 (txt "(piccolo)" UP -0.2))
(x-rm part break 27 D 42))

\markup { \vspace  #2 }

\new Staff { $(instru->music part "bass") }

\markup { \vspace  #15 }
