%% All these functions are described in the addendum 2 and 3 of the documentation of arranger.ly

#(define (part? arg)
(and (defined? 'part)
     (if (list? arg)(memq part arg)
                    (eq? part arg))))

#(define score? (part? 'score))

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% From her, a variable called assocDynList must have been defined with assoc-pos-dyn
%    assocDynList = #(assoc-pos-dyn ...)

%%% 1 instru in a Staff
#(define* (instru->music instru #:optional (clef "treble"))
(sim                   ; << ... >> (simultaneous)
  (make-clef-set clef) ; = \clef
  global
  (obj->music instru)  ; music of instru
  (add-dyn instru)))   % dynamics of instru

%% 2 instrus in the same Staff
#(define* (split-instru instru1 instru2 #:optional (clef "treble"))
   (split                    ; << … \\ … >>
      (sim                   ; << … >>
         (make-clef-set clef)
         global
         dynamicUp
         (add-dyn (list 'xor instru1 instru2))
         (obj->music instru1))
      (sim
         (add-dyn instru2)
         (obj->music instru2))))

#(define* (part-combine-instru instru1 instru2 #:optional (clef "treble"))
   (sim
     (make-clef-set clef)
     global
     (part-combine           ; \partCombine
       (sim                  ; instru1 : upper voice
         ;partCombineApart        ; for working
         partCombineAutomatic     ; default
         dynamicUp                ; dynamics above Staff
         (add-dyn (list 'xor instru1 instru2)) ; dyn only if no instru2 dyn
         (obj->music instru1))    ; instru1 music
       (obj->music instru2)) ; instru2 : lower voice music
     (add-dyn instru2)))          % instru2 dynamics

%%%%%%%%%%%%%%%%%%%
% For dynamics

#(define ((set-dyn fmt0 . fmt-args) arg0 . args)
   (apply format (lst #f fmt0 fmt-args arg0 (filter not-procedure? args))))

#(define (x-dyn fmt) (set-dyn "~@{~}" (string-append fmt "~^ / ")))

#(define (bar-offset bar-numbers offsets)
"(bar-offset '(b1 b2...bi) '(o1 o2 ...oj)) returns the list :
b1 + o1, b1 + o2,...b1 + oj, b2 + o1, b2 + o2,...b2 + oj ... bi + oj"
(fold-right
  (lambda(b prev1)
    (fold-right
      (lambda(o prev2) (cons (+ b o) prev2))
      prev1
      offsets))
  '()
  bar-numbers))

%{ example from doc arranger.ly (addendum 3)
#(define (dyn<> . bar-nums )
   (apply (x-dyn "\n~a < / (~a 8) > / (~a 4 16) !")
          (bar-offset bar-nums '(0 2 3))))
#(write (dyn<> 5 11 20))
%}

% macro to define objects from inside assoc-pos-dyn
#(define-macro (def-dyn dyn arg) `(begin (define ,dyn ,arg) /))