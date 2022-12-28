%% set-transp-func-fr.ly
\version "2.24.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\layout {
  \context {
    \Score
    \override BarNumber.font-size = #+2
  }
}

global = s1*7
all = #'(I II III)
#(init all)

music = \relative { c'4 d e f g a  b c
                    e dis e ees d cis d des c b bes a gis a aes g
                    c,1 \bar "|." }
fromPitch = c'
%fromPitch = d'
% fromPitch = bes
toPitchI = a
toPitchII = c'

music = $(ly:music-transpose music fromPitch) % for testing other scales


#(define (maj->min from-pitch to-pitch) ;
(let((delta (ly:pitch-diff to-pitch from-pitch))
     (special-pitches (music-pitches
         (ly:music-transpose #{ dis e eis gis a ais #} from-pitch))))
  (lambda(p)  ; return the delta pitch for the transposition of p
    (ly:make-pitch
      0
      (ly:pitch-steps delta)
      (+ (ly:pitch-alteration delta)
         (if (find (same-pitch-as p 'any-octave) special-pitches)
           -1/2
           0))))))

#(begin
(rm all 1 music)                                ; c \major
(apply-to 'II (set-transp (maj->min fromPitch toPitchI)) 1 8)   ; a minor
(apply-to 'III (set-transp (maj->min fromPitch toPitchII)) 1 8)   ; c minor
)
 \score {
   <<
     \new Staff \with { instrumentName = "I" } 
        $(sim (txt (markup #:underline "Do majeur") UP) I)
     \new Staff \with { instrumentName = "II" }
        $(sim (txt (markup #:underline "La mineur") UP) II)
     \new Staff \with { instrumentName = "III" } 
       $(sim (txt (markup #:underline "Do mineur") UP) III)
   >>
   \layout { indent = 0 }
 }

