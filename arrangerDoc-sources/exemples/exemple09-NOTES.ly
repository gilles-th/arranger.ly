%% exemple09

\version "2.19.83"

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
#(begin
(rm 'vl 1 music)
(add-voice2 'fl 3 (adef (em vl 3 4) "(violon)" DOWN))
(rm 'fl 4 (seq (txt "obligé" UP)
               (rel #{ f'4 g a b | c1 #}))))

%   \new StaffGroup <<
%   \new Staff \with{ instrumentName = fl }{<< \global \fl >>}
%   \new Staff \with{ instrumentName = vl }{<< \global \vl >>}
%   >>
