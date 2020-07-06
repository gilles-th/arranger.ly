#(define part 'xylo)
\version "2.19.83"
\include "../NOTES.ily"

\header { instrument = "xylophone" }

\paper { 
  min-systems-per-page = 8
}

#(let ((m (em tp1 25 27)))
(add-voice1 part 25 (adef m  "(tpttes)" DOWN))
(rm-with part 
  3 (txt (markup #:fontsize adef-size "(clar solo)") UP -0.2)
 11 (txt (markup #:fontsize adef-size "(piccolo)") UP -0.2))
(x-rm part break 11 (+ C 8) 31 D (+ D 7))
)


\markup \vspace #1

\new Staff { $(instru->music part) }

\markup \vspace #1