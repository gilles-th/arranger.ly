\version "2.25.7"
\include "arranger.ly"

#(ly:set-option 'crop #t)

global = s1*2
#(init '(mydrums))

#(set! lilypond-mode 'drummode)

#(begin ;;
(fill 'mydrums "hihat8" 1 3)
(add-voice2 'mydrums 1 "bassdrum8 r r4")
(add-voice2 'mydrums 2 "r8 bassdrum r4")
)

\new DrumStaff $mydrums
