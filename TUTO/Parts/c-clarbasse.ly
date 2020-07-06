#(define part 'clB)
\version "2.19.83"
\include "../NOTES.ily"

\header { instrument = "Clarinette basse" }

\paper {
  min-systems-per-page = 9
}

\layout {
  \context {
    \Staff
    \consists #Measure_counter_engraver
  }
}

startMeasureCount = {
  \override Staff.MeasureCounter.count-from = #2
  \startMeasureCount
}

#(begin
(rm part 3 (txt "(clar solo)" UP))
(rm part 11 (txt "(piccolo-fl solo)" UP))
(x-rm part startMeasureCount 4 12)
(x-rm part stopMeasureCount 11 C)

)

\markup { \vspace  #1 }

\new Staff \transpose c d' { $(instru->music part) }

\markup { \vspace  #8 }
