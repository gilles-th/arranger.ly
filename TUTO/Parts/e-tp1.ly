#(define part 'tp1)
\version "2.24.0"
\include "../NOTES.ily"

\header { instrument = "Trompette 1" }

\paper {
  min-systems-per-page = 12
}

dynUpOn = \override Score.DynamicLineSpanner.direction = #UP
dynUpOff = \revert Score.DynamicLineSpanner.direction
rotation = \override Score.Hairpin.rotation = \etc
extraOffset = \override Score.Hairpin.extra-offset = \etc
revertHairpin = { \revert Score.Hairpin.extra-offset
                  \revert Score.Hairpin.rotation }

#(begin
(rm part 3 (txt "(clar solo)" UP -0.2))
(x-rm part break 11 C D 42)
; m.39, tp1 is divided : hairpins are for voice 1 only (-> UP)
(merge-in-with part ; << >>
   D #{ s2 \dynUpOn s2 s1 \dynUpOff #} ; free up spaces for m.39
  39 #{ s8
        \dynUpOn
        \rotation #'(7 -1 0)          % Hairpin overrides
        s8 s4
        \rotation #'(-3 0 0) \extraOffset #'(2 . -0.5)
        s2
        \revertHairpin
        \dynUpOff #}))

\new Staff \transpose c d { $(instru->music part) }

