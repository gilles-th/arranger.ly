%% exemple07.ly
\version "2.20.0"

\include "arranger.ly"

\paper  {
  %ragged-right = ##t
  line-width=120\mm
  indent = 0
}

#(ly:set-option 'crop #t)

global = { \time 6/8 s8*6*4 }


#(begin ;;
(init '(test))
(rm 'test 1 (cp #{ r8 c16 c c8 c c c #}
                (tweak-notes-seq
                   (list 1 2 3 (cons 1 (set-octave +1)) 3 2)
                   (rel 1 #{ c8 e g | a, c e | f, a c | g b d #}))))
)
%#(display (octave 1 #{ d8 f a c e g #}))

\new Staff { << \global \test { s2.*2 \break } >> }

