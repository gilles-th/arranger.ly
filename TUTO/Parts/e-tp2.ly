#(define part 'tp2)
\version "2.19.83"
\include "../NOTES.ily"

\header { instrument = "Trompette 2" }

\paper {
  min-systems-per-page = 9
  ragged-last-bottom = ##f
}

#(begin
(rm part 3 (txt "(clar solo)" UP -0.2))
(x-rm part break 11 C D 42))

\markup \vspace #1

\new Staff \transpose c d { $(instru->music part) }

\markup \vspace #7