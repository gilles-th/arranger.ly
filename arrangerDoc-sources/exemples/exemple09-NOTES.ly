%% exemple09-NOTES.ly
\version "2.20.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\layout { \context { \Score
  \override BarNumber.break-visibility = #all-visible
  barNumberVisibility = #all-bar-numbers-visible
  skipBars = ##t
  \override BarNumber.font-size = #+2
  \override MultiMeasureRest.expand-limit = #1
  }
}
global = {s1*5 \bar "|."}
all = #'(fl vl)
#(init all)
music = \relative c' { 
  e'2 d | c b | 
  a4 g f e | d2 g, | c1
}

#(if (not (defined? 'names))
  (define names (list "(violon)" "obligé")))

#(begin
(rm 'vl 1 music)
(add-voice2 'fl 3 (adef (em vl 3 4) (first names) DOWN))
(rm 'fl 4 (seq (txt (second names) UP)
               (rel #{ f'4 g a b | c1 #}))))

%   \new StaffGroup <<
%   \new Staff \with{ instrumentName = fl }{<< \global \fl >>}
%   \new Staff \with{ instrumentName = vl }{<< \global \vl >>}
%   >>
