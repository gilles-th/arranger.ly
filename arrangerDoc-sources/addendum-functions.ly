%% All these functions are described in the addendum 2 and 3 of the documentation of arranger.ly

#(define (part? arg)
(and (defined? 'part)
     (if (list? arg)(memq part arg)
                    (eq? part arg))))

#(define score? (part? 'score))

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% From here, a variable called assocDynList must have been defined with assoc-pos-dyn
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
% See  annex below for some fmt args formatting

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

%%%%%%%%%%%%%%%%% macros to be used inside assocDynList %%%%%%%%%%%
%%  each macro returns the guile divide function / because assoc-pos-dyn trims it.
#(define-macro (def-dyn dyn arg) `(begin (define ,dyn ,arg) /))
#(define-macro (def-dyn+txt dyn txt) ; (def-dyn+txt "p" "sub") => new dynamic psub
`(let ((sym (string->symbol (string-append ,dyn ,txt))))
   (ly:parser-define! sym (make-dynamic-script (markup
                            #:dynamic ,dyn ; #:hspace 0.3
                            #:normal-text #:italic ,txt)))
   /))
#(define-macro (def-txt+dyn txt dyn) ; (def-txt+dyn "molto" "f") => new dynamic moltof
`(let ((sym (string->symbol (string-append ,dyn ,txt))))
   (ly:parser-define! sym (make-dynamic-script (markup
                            #:normal-text #:italic ,txt ; #:hspace 0.3
                            #:dynamic ,dyn)))
   /))
%%%%%% def-span-dyn macro.%%%%%%
  %% a pre utility function
  #(define (def-span-dyn-generic sym txt)
     (ly:parser-define! sym (make-music 'CrescendoEvent
       'span-direction START 'span-type 'text 'span-text txt))
     /)
  %% the macro :
% syntax : (def-span-dyn sym txt)
% sym can be omitted : the macro then gets it from the letters of txt
%   (def-span-dyn "cresc. molto") => new crescmolto span-dynamic
%
% Note1 : the syntax (pair? ,args) does not work in a macro
% => work around found here : (pair? (list ,@args))
% Note2 : According to the guile help, syntax for string-filter is:
% (string-filter pred str) but it seems to be (string-filter str pred) !
#(define-macro (def-span-dyn txt . args)
`(if (and (pair? (list ,@args)) (symbol? ,txt))
   (apply def-span-dyn-generic (list ,txt ,@args))
   (let ((sym (string->symbol (string-filter ,txt char-set:letter))))
     (def-span-dyn-generic sym ,txt))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% annex %%%%
%% reminder for the guile format argument, used in fmt args of set-dyn and x-dyn
%%  ~a    -> convert to string arg0 then in next call, arg1 then arg2 etc
%%              (format #f "~a ~a ~a" 1 2 3) => 1 2 3
%%  ~N*   ->  moves N arguments FOWARD from current arg (N defaults to 1 if omitted)
%%              (format #f "~a ~2*~a" 1 2 3 4) => "1 4"
%%  ~:N*  -> moves N arguments BACKWARDS from current arg : N defaults to 1
%%              (format #f "~a ~a ~:2*~a" 1 2 3 4) => "1 2 1"
%%  ~N@*  -> moves to argument number N (1st arg index 0, 2nd index 1 etc...)
%%              (format #f "~a~a~a ~1@*~a~a~a" 1 2 3 4)  ⇒ "123 234"


%}