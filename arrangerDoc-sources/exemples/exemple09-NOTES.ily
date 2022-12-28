% exemple09-NOTES.ly
% this file is included in several ly files and doesn't produce any pdf
\version "2.24.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\layout { \context { 
  \Score
  \override BarNumber.break-visibility = ##(#f #t #f)
  \override BarNumber.self-alignment-X = #CENTER
  skipBars = ##t
  \override BarNumber.font-size = #+1
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

% exemple09-flute-en.ly has an english version of names below.
#(if (not (defined? 'names))
  (ly:parser-define! 'names (list "(violon)" "obligé")))

#(begin
(rm 'vl 1 music)
(add-voice2 'fl 3 (adef (em vl 3 4) (first names) DOWN))
(rm 'fl 4 (seq (txt (second names) UP)
               (rel #{ f'4 g a b | c1 #}))))


