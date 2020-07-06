#(define part 'htb)
\version "2.19.83"
\include "../NOTES.ily"

\header { instrument = "Hautbois" }

\paper {
  min-systems-per-page = 9
  ragged-last-bottom = ##f
}

#(let ((m (em fl1 18 19)))
(add-voice1 part 18  (adef m "(fl1)" UP -0.8))
(rm-with part
  3 (txt (markup #:fontsize adef-size "(clar solo)") UP -0.2)
 11 (txt (markup #:fontsize adef-size "(piccolo)") UP -0.2)))

\markup \vspace #2

\new Staff { $(instru->music part) }

\markup \vspace #6
