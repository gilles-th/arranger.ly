#(define part 'cl3)
\version "2.24.0"
\include "../NOTES.ily"

\paper { 
  min-systems-per-page = 12
}

\header { instrument = "clarinette 3" }

\markup { \vspace  #1 }

\new Staff \transpose c d { $(instru->music part) }

\markup { \vspace  #1 }