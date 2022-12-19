#(define part 'alt1)
\version "2.24.0"
\include "../NOTES.ily"

\header { instrument = "Saxo alto 1" }

\paper {
  min-systems-per-page = #13
}

#(let ((m (em (voice 1 cl1) 10 11)))
(add-voice1 part 10  (adef m "(clar solo)" UP 1.2))
(rm part 3 (txt (markup #:fontsize adef-size "(clar solo)") UP -0.2))
(rm part 45 #{ \override Score.DynamicTextSpanner.minimum-length = #11 #}) ; for \cresc
(x-rm part break 11 C D 42)
)

#(def! 'altoI
   (let ((m (instru->music part)) ;; (sim global alt1 (add-dyn 'alt1) ...)
         (m40 (em alt1 40 41))
         (dyn40-alt1 (em (add-dyn 'alt1) 40 41))
         (dyn40-alt2 (em (add-dyn 'alt2) 40 41)))
     (rm m 40 (split (sim (voice 1 m40) dynamicUp dyn40-alt1)
                     (sim (voice 2 m40) dyn40-alt2)))))

% When using rm with \global, the last event after the last skip (here \bar "|.")
% disappears ! We add it by hand.

\new Staff \transpose es c' { \altoI \bar "|." }