%% macro-spanner-dynamics.ly
\version "2.24.0"

\include "arranger.ly"

#(ly:set-option 'crop #t)

\layout {
  indent = 0
  \context { \Score  
    skipBars = ##t
    \override MultiMeasureRest.expand-limit = #1
  }
}

% Pre-function
#(define (def-span-dyn-generic sym txt)
     (ly:parser-define! sym (make-music 'CrescendoEvent
       'span-direction START 'span-type 'text 'span-text txt))
     /)
% The macro
#(define-macro (def-span-dyn txt . args)
`(if (and (pair? (list ,@args)) (symbol? ,txt))
   (apply def-span-dyn-generic (list ,txt ,@args))
   (let ((sym (string->symbol (string-filter char-set:letter ,txt))))
     (def-span-dyn-generic sym ,txt))))
 
            %%%%%%%%%%%%

global = s1*3
music = { \repeat unfold 16 c8 c1 }

#(init '(instru))
#(rm 'instru 1 (rel 1  music))

assocDynList = #(assoc-pos-dyn
  (def-span-dyn 'crescspace " ")
  (def-span-dyn "cresc. molto")
    "1 pp crescspace / 2 crescmolto / 3 ff" 'instru
)

\score {
     \new Staff $(sim global instru (add-dyn 'instru))
}
