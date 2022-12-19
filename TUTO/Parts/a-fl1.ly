#(define part 'fl1)
\version "2.24.0"
\include "../NOTES.ily"

\header { instrument = "Flûte 1" }

\paper {
  min-systems-per-page = 13
}

#(let ((m (em piccolo 11 12))
       (del-txt (set-del-events 'TextScriptEvent))) ; del "solo" txt
(add-voice1 part 11 (adef (octave -1 (del-txt m)) "(piccolo)"))
(rm part 3 (txt (markup #:fontsize adef-size "(clar solo)") UP -0.2))
(x-rm part break 11 C))

\markup \vspace #1

\new Staff { $(instru->music part) }

\markup \vspace #1
