%% addendum-1
\version "2.20.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\layout {
  \context { \Score
    skipBars = ##t
    \override MultiMeasureRest.expand-limit = #1
    markFormatter = #format-mark-box-letters
  }
}

global = { s1*1000 } %% On prévoit une grande longueur
#(init '())          %% Liste d'instruments d'abord vide =>
%% les positions tiennent compte des insertions précédentes de timing.
%% ( \global est ré-analysé à chaque fois. )
#(begin ;; Construction de \global
(signatures 1 "3/4" 10 "5/8" 20 "4/4") ;; D'abord les signatures rythmiques
(cut-end 'global 70)                   ;; On coupe ce qui est en trop
(keys 1 "d minor" 20 "bes major" 30 "d major") ;; Les armures
(tempos                                ;; Les indications de tempos
   1 (metronome "Allegro" "4" 120) /
  10 (metronome "" "8" "8") 2 /       ;; décalage de 2 unités vers la droite
  20 (metronome "Allargando" "4" "4.") 2.5 /
  30 "Piu mosso" -4 /
  60 (markup #:column ("FINAL" (metronome "Allegro vivo" "4" 200))))
(marks 10 20 30 40 50 60)          ;; Les lettres 
(x-rm 'global (bar "||") 20 30 60) ;; Les barres
(rm-with 'global 1 markLengthOn    ;; Choses diverses
                40 break          
                70 (bar "|."))    ;; La touche finale
)                                 %% Fin \global

%% On peut maintenant initialiser la liste d'instruments
#(init '(test)) %% Liste non vide = métrique fixée : tout nouveau timing sera ignoré
\new Staff { << \global \test >> }

