#(define part 'timbales)

\version "2.24.0"
\include "../NOTES.ily"

\header { instrument = "Timbales" }

#(begin
(rm-with part
   3 (txt "(clar solo)" UP)
  11 (txt "(piccolo)" UP))
(x-rm part break 11 C 27 D 42))

\markup { \vspace #1 }
\markup \timbalesTuning #"<f, c>^\"accord :\"" #-3
\markup { \vspace #1 }

\new Staff { $(instru->music part "bass") }

\markup { \vspace  #16 }
