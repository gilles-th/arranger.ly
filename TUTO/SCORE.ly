#(define part 'score)
\include "NOTES.ily"

#(set-global-staff-size 15)

\paper { ragged-right = ##f }

\header { instrument = "Conducteur UT" } % score in concert pitch

%%%%%%%%% settings of the 3 Dynamics staves (used for bar-numbers, marks, tempos)

dynWithUp = \with { % staff in the top of score
  % Page 6 for ex., flutes have high pitches ! It is ugly to keep  break-visibility to all-visible
  \override BarNumber.break-visibility = ##(#f #f #t)
  \override BarNumber.extra-offset = #'(-1.2 . -1.8)
  \override  VerticalAxisGroup.nonstaff-relatedstaff-spacing =
     #'((basic-distance . 1)
        (minimum-distance . 5)
        (padding . 1)
        (stretchability . 100))
}

dynWithMid = \with { % staff in the middle of score
  %% At beginning of each systems, the bar number overlaps with the SystemBarLine :
  \override BarNumber.layer = #2    % barNumber will be drawn after system bars
  \override BarNumber.whiteout = #6 % erase the segment of vertical line under the bar number
  \override BarNumber.whiteout-style = #'rounded-box % a circle box with extra space of 6 (parhaps 6/2 ?)
  \override  VerticalAxisGroup.nonstaff-relatedstaff-spacing =
     	#'((basic-distance . 1)
     	   (minimum-distance . 1)
     	   (padding . 2)
     	   (stretchability . 100))
  \override VerticalAxisGroup.staff-affinity= #CENTER
}

dynWithDown = \with { % staff in the bottom of score
  \override  VerticalAxisGroup.nonstaff-relatedstaff-spacing =
     #'((basic-distance . 1)
        (minimum-distance . 3)
        (padding . 2)
        (stretchability . 100))
}

%%%%%%%%% instruments hierarchies

flHtbHierarchy = \with { systemStartDelimiterHierarchy =
  #'(SystemStartBracket (SystemStartBrace piccolo (SystemStartSquare fl12))
                        htb)  }
clarHierarchy = \with { systemStartDelimiterHierarchy =
  #'(SystemStartBracket (SystemStartBrace cl1 (SystemStartSquare cl23))
                        (SystemStartSquare bsn-clB)) }
saxosHierarchy = \with { systemStartDelimiterHierarchy =
  #'(SystemStartBracket (SystemStartSquare alt12)
                        altT
                        saxB) }
brassHierarchy = \with { systemStartDelimiterHierarchy =
  #'(SystemStartBracket (SystemStartBrace tp1 (SystemStartSquare tp23))
                        (SystemStartSquare cors)
                        (SystemStartSquare tbs)
                        (SystemStartSquare tubas)) }

%%%%%%%%% the score
% each staff use either instru->music or either split-instru . Their are defined in
% the end of init.ily

\score {
<<
  %%%%% flutes htbois %%%%
  \new Dynamics \with { \dynWithUp } \global
  \new StaffGroup \with { \flHtbHierarchy } <<
    \new GrandStaff \with { shortInstrumentName = \markup \char-column #"fl" #2 }  <<
      \new Staff \with { instrumentName = "piccolo" }
         $(instru->music 'piccolo "treble^8")
      \new Staff \with { instrumentName = \markup \vcenter { "flute " \column { 1 2 }}}
         $(split-instru 'fl1 'fl2)
      >>
    \new Staff \with { instrumentName = "htbois"
                       shortInstrumentName = \markup \char-column #"htb" #1.6 }
         $(instru->music 'htb)
  >> %% end StaffGroup fls htb
  %%%%% clars (+ bsn) %%%%
  \new StaffGroup \with { \clarHierarchy } <<
    \new GrandStaff \with { shortInstrumentName = \markup \char-column #"cl" #2 } <<
      \new Staff \with { instrumentName = "clar 1"}
        $(instru->music 'cl1)
      \new Staff \with { instrumentName = \markup \vcenter { "clar " \column { 2 3 }}}
        $(split-instru 'cl2 'cl3)
      >>
    \new GrandStaff \with { shortInstrumentName = \markup \char-column #"bas" #1.4 } <<
      \new Staff \with { instrumentName = \markup \center-column {"basson" "clar basse"}}
        $(split-instru 'bsn 'clB "bass")
    >>
  >> %% end StaffGroup clars
  %%%%% sax %%%%
  \new StaffGroup \with { shortInstrumentName = \markup \char-column #"saxos" #4
                          \saxosHierarchy } <<
    \new GrandStaff  <<
      \new Staff \with { instrumentName = \markup \vcenter { "alto " \column { 1 2 }}}
        $(split-instru 'alt1 'alt2)
    >>
    \new Staff \with { instrumentName = "tenor" }
        $(instru->music 'saxT "G_8")
    \new Staff \with { instrumentName = "baryton" }
        $(instru->music 'saxB "bass")
  >>
  \new Dynamics \with { \dynWithMid } \global
  %%%%% cuivres %%%%
  \new StaffGroup \with { \brassHierarchy }
    << \new GrandStaff \with { shortInstrumentName = \markup \char-column #"tp" #2 }  <<
         \new Staff \with { instrumentName = "tptte 1" }
           $(instru->music 'tp1)
         \new Staff \with { instrumentName = \markup \vcenter { "tptte " \column { 2 3 }}}
           $(split-instru 'tp2 'tp3)
       >>
       \new GrandStaff \with { shortInstrumentName = \markup \char-column #"cor" #1.5 } <<
         \new Staff \with { instrumentName = \markup \vcenter { "cor " \column { 1 2 }}}
           $(split-instru 'cor1 'cor2)
       >>
       \new GrandStaff \with { shortInstrumentName = \markup \char-column #"tbn" #1.5 } <<
         \new Staff \with { instrumentName = \markup \vcenter { "tbne " \column { 1 2 }}}
           $(split-instru 'tb1 'tb2 "bass")
       >>
       \new GrandStaff \with { shortInstrumentName = \markup \char-column #"tba" #1.5 } <<
         \new Staff \with { instrumentName = \markup \center-column {"euphonium" "tuba"} }
           $(split-instru 'euph 'tuba "bass")
       >>
    >>

  %%%%% ctreBasse %%%%
  \new Staff \with { instrumentName = \markup \center-column { "contrebasse " "à " "cordes"}
                     shortInstrumentName = \markup \char-column #"ctb" #1.5 }
    $(instru->music 'ctb "bass_8")
  %%%%% percu %%%%
  \new StaffGroup \with { shortInstrumentName = \markup \char-column #"percus" #4 }  <<
    \new Staff \with { instrumentName = "xylo" }
      $(instru->music 'xylo)
    \new DrumStaff \with { instrumentName = "accessoires" }
      \drummode {  $(instru->music 'percu "percussion")  }    % << \percu >>
    \new Staff \with { instrumentName = "timbales" }
      $(instru->music 'timbales "bass")
    >>
  \new Dynamics \with { \dynWithDown } \global
>>
\layout {
  indent = 13
  \context {
    \Score
    \remove "Mark_engraver"
    \remove "Staff_collecting_engraver"
    \remove "Metronome_mark_engraver"
    \remove "Bar_number_engraver"
  }
  \context {
    \Dynamics
    \consists "Staff_collecting_engraver"

    \consists "Mark_engraver"
    %\override RehearsalMark.font-size = #1
    \override RehearsalMark.self-alignment-X = #CENTER
    \override RehearsalMark.self-alignment-Y = #CENTER
    \override RehearsalMark.Y-offset = #ly:self-alignment-interface::y-aligned-on-self

    \consists "Metronome_mark_engraver"
    \override MetronomeMark.self-alignment-Y = #CENTER
    \override MetronomeMark.Y-offset = #ly:self-alignment-interface::y-aligned-on-self

    \consists "Bar_number_engraver"
    \override BarNumber.break-visibility = #all-visible
    \override BarNumber.font-series = #'bold
    \override BarNumber.self-alignment-X = #CENTER %% X center on bar
    \override BarNumber.self-alignment-Y = #CENTER %% Y center in the Dynamics Staff
    \override BarNumber.Y-offset = #ly:self-alignment-interface::y-aligned-on-self
  }
  \context {
    \GrandStaff % only to have the property shortInstrumentName
    \remove  "System_start_delimiter_engraver"
  }
  \context {
    \StaffGroup
    \override SystemStartBracket #'collapse-height = #1
    \override SystemStartBrace #'collapse-height = #1
    \override SystemStartBar #'collapse-height = #1
    \override SystemStartSquare #'collapse-height = #1
  }
} % layout
} % score

\version "2.20.0"