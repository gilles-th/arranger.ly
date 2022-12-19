#(define part 'percu-score)
#(define header-sep 15)
\version "2.24.0"
\include "../NOTES.ily"

%%#(set-global-staff-size 16)

\header { instrument = "Percussions"
          instrumentMore = \markup \vcenter {
            \override #'(font-encoding . fetaBraces) {
              \lookup "brace160"
              \hspace #1 }
            \tiny \override #'(baseline-skip . 5) \column {
              Xylophone
              \line { Tambourin + Triangle }
              \line \vcenter{  "Timbales : "  \timbalesTuning #"<f, c>" #-3 }
            }
          }
}

\paper {
  min-systems-per-page = 5
%  page-count = 2
 % ragged-last-bottom = ##f
}

\layout {
  indent = 20
  \context {
    \Score
    \override VerticalAxisGroup.remove-first = ##f
    \override SystemStartBar.collapse-height = #1
    \override BarNumber.extra-offset = #'(-1.2 . 1.2) % the bar number colllides with the bracket
  }
  \context {
    \StaffGroup
    \override SystemStartBracket.collapse-height = #1
  }
  \context {
    \Staff
    \RemoveEmptyStaves
  }
  \context {
    \RhythmicStaff
    \RemoveEmptyStaves
  }
 }

showMultiRests = \set Staff.keepAliveInterfaces = #'(multi-measure-rest-interface)
hideMultiRests = \unset Staff.keepAliveInterfaces
#(define (rhythmicStaffName name baseline)
   #{ \set RhythmicStaff.shortInstrumentName = #(markup #:tiny #:char-column name baseline) #})
textNoPriority= \once \override TextScript.outside-staff-priority = ##f % C : "o" must be below "tambourin" !
lineCountFive = { % D : impossible to get SystemStartBracket working if line-count = 1
  \stopStaff
  \override RhythmicStaff.StaffSymbol.line-count = #5
  \startStaff
  \bar "|"
}
lineCountOne = {
  \stopStaff
  \revert RhythmicStaff.StaffSymbol.line-count
  \startStaff
  \once \override Score.BarLine.bar-extent  = #'(-2 . 2) % line too short
  \bar "|"
}
boxIt = \once \override Score.TextScript.stencil = #(lambda(grob) ; "tambourin" and "triangle" in a box
          (let ((text (ly:grob-property grob 'text #f)))
            (grob-interpret-markup grob (markup #:box text))))
cross = \override NoteHead.style = #'cross

#(begin
(x-rm 'percu boxIt C 42)
(x-rm 'global break C 27 D (+ D 7))
(rm 'global 'end #{ \bar "|." #})
(rm-with 'percu
   ;  3 (txt "(clar solo)" UP CENTER)
   ; 11 (txt "(piccolo)" UP -0.2)
   '(19 8) textNoPriority
      D (seq showMultiRests (rhythmicStaffName "percus" 1.3) lineCountFive)
     42 (seq lineCountOne (rhythmicStaffName "tri" 1.5) stemDown cross))
)

%%%%%%%%%%%%%%%%%%%%%%%%
\markup \vspace #1.5

\score {
  \new StaffGroup
  <<
	   \new Staff  \with { instrumentName = \markup \small "xylophone"
		                   shortInstrumentName = \markup \tiny \char-column #"xylo" #1.5 }
          { $(instru->music 'xylo) }

       \new RhythmicStaff \with {
                              \override StaffSymbol.line-count = #1
                              instrumentName = \markup \override #'(baseline-skip . 1.7) \center-column
                                                  \small {tambourin + triangle}
		                      shortInstrumentName = \markup \tiny \char-column #"tbin" #1 }
           { $(instru->music 'percu "percussion") }

	   \new Staff \with { instrumentName = \markup \small "timbales"
		                  shortInstrumentName = \markup \tiny \char-column #"tbal" #1 }
          { $(instru->music 'timbales "bass") }

   >>
}

\markup \vspace #5
