#(define part 'euph)
\version "2.24.0"
\include "../NOTES.ily"

\header { instrument = "Euphonium Sib 1 et 2" }

\paper {
  min-systems-per-page = 9
}



#(let ((m (em cl1 17 19)))
(add-voice2 part 17  (adef m (markup #:fontsize (- adef-size) "(clar-tpttes)") DOWN -0.5))
(rm-with part
   3 (txt "(clar solo)" UP CENTER)
  11 (txt "(piccolo)" UP -0.2))

(x-rm part break C 27 D)
(apply-to part (set-del-events 'TextScriptEvent) D 36) ; remove "div" TextScript

(def! 'euph2 global)  ; new instrument initialized by global
((copy-to-with-func (set-note 1)) 'euph2 euph C 27 / D 'end)
  ; ↳ copy each sections, taking 1st note of chords
(def! 'euph (note 2 euph)) ; 2nd in chords

(x-rm 'euph (txt "1°" UP) C D)
(x-rm 'euph2 (txt "2°" UP) C D)
(rm 'euph 27 (txt "uni" UP)))

\markup { \vspace  #1 }

\score {
  \new GrandStaff <<
   \new Staff \transpose c d' { $(instru->music 'euph) }
   \new Staff \transpose c d' { $(sim euph2 (add-dyn 'euph)) }
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

\markup { \vspace  #4 }
