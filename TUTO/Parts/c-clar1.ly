#(define part 'cl1)
\version "2.19.83"
\include "../NOTES.ily"

\header { instrument = "clarinette 1 + solo " }

\paper {
  min-systems-per-page = 9
}

#(begin
(def! 'clsolo)                        ; clsolo = s1*45
(copy-to 'clsolo (voice 1 cl1) 3 11)  ; extract upper voice from m.3 to m.16
(apply-to 'cl1 (set-voice 2) 3 11)    ; remove upper voice
(x-rm 'clsolo break 3 11)
  )

\score {
  \new GrandStaff <<
   \new Staff \transpose c d { $(sim global clsolo (add-dyn 'cl1)) }
   \new Staff \transpose c d { $(instru->music 'cl1) }
>>
\layout {
  \context {
    \Score
    \override VerticalAxisGroup.remove-first = ##t
  }
  \context {
    \Staff
    \RemoveEmptyStaves
  }
 }
}


%\markup \vspace #10
