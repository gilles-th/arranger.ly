%% exemple10
\version "2.19.83"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\paper{
  indent=0\mm
  line-width=120\mm
}

\layout { \context { \Score
  markFormatter = #format-mark-box-barnumbers
  \override RehearsalMark.break-visibility = #'#(#t #t #f)
  \override RehearsalMark.font-size = #-1
  skipBars = ##t
  %\override BarNumber.font-size = #+2
  \override MultiMeasureRest.expand-limit = #1
  }
}

global = { s1*1000 }
#(init '())

#(begin
(rm-with 'global 1 #{ \time 3/4 #} / 
                10 #{ \time 5/8 #} /
                20 #{ \time 4/4 #})
(cut-end 'global 70)
(x-rm 'global #{ \mark \default #} 10 20 30 40 50 60)
(tempos 1 (metronome "Allegro" "4" 120) / 
       10 (metronome "" "8" "8") /
       20 (metronome "Allargando" "4" "4.")
       30 "Piu mosso"
       60 (markup #:column ("FINAL" (metronome "Allegro vivo" "4" 144))))
(rm-with 'global 1 #{ \key c \major #} / 
                20 #{ \key c \minor #} /
                30 #{ \key c \major #})
(x-rm 'global #{ \bar "||" #} 20 30 60)
(rm-with 'global 1 #{ \markLengthOn #}
    40 #{ \break #})
(rm 'global 70 #{ \bar "|." #})
)

#(init '(test))

\new Staff { << \global \test >> }
