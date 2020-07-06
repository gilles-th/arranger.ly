#(define part 'cl2)
\version "2.19.83"
\include "../NOTES.ily"

\paper { 
  min-systems-per-page = 12
}

\header { instrument = "clarinette 2" }

\markup { \vspace  #1 }

\new Staff \transpose c d { $(instru->music part) }

\markup { \vspace  #1 }