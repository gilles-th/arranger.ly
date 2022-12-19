#(define part 'percu)
\version "2.24.0"
\include "../NOTES.ily"

\header { instrument = "tambourin + triangle" }

\layout { \context {   % bar number and mark not vertically aligned with default settings
            \Score
              \override Clef.break-align-anchor-alignment = #2 } }

boxIt = \once \override Score.TextScript.stencil = #(lambda(grob) ; tambourin and triangle in a box
          (let ((text (ly:grob-property grob 'text #f)))
            (grob-interpret-markup grob (markup #:box text))))
textNoPriority= \once \override TextScript.outside-staff-priority = ##f % C : "o" must be below "tambourin" !
textToTop = \once \override TextScript.outside-staff-priority = #10000  % 42 : "Triangle" above "Accelerando"

#(rm-with part
   3 (txt "(clar solo)" UP CENTER)
  11 (txt "(piccolo)" UP -0.2)
'(19 8) textNoPriority
  42 textToTop)
#(x-rm part boxIt C 42)
#(x-rm part break C (+ C 4) 27 31 D (+ D 7))

\new DrumStaff \drummode { $(instru->music 'percu "percussion") }

\markup { \vspace  #10 }