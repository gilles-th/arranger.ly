%% exemple06.ly
\version "2.24.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

music =
 <<
{ e8
 e e e e e e e} \\
{ c4
 d8 c c4 d8 c } >>
percu =
 #(pitches->percu music 'hihat /
#{ c #} 'bassdrum /
#{ d #} 'snare)
\new DrumStaff \drummode { \percu }
