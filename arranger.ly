\version "2.24.0"

%%%%%%%%%%%%%%%%%%%%%% version Y/M/D = 2022/12/16 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main objective of arranger.ly is to provide a set of functions to make
% arrangements of pieces originally intended for another band :
% for ex a symphonic piece arranged for a concert band (wood-winds and percussions only).
% In particular, arranger.ly allows the insertion of a fragment of music (a theme for ex),
% to simultaneously a whole set of instruments, and to several musical positions in one shot.
% To achieve this goal, the notion of "position" had to be rethought. The timing
% location system is now based on measure number (Lilypond use instead moments).
% To use arranger.ly functions, a user has just to :
% 1- define a variable called \global, where all time signatures are stored.
%   ex:      global = { s1*4 \time 5/8 s8*5*7 \time 3/4 s4*3*6 ...}
%   (but others timing events like \partial, cadenzaOn/Off... are also supported)
% 2- call the init function (see later) with a set of instruments as parameter.
% The immediat result is that each instruments are automatically filled by appropriate
% multi-measure rests (same length than \global). (Even a starting rest is added if a
% \partial is found in \global). Then the insertion of musics can begin.
% last changes (Most recent at the top):
%   add-dynamics supports also now \afterGrace section
%   syntax extension for x-pos function
%   voice function allows more parameters (voice n [m ..] music)
%   extended features for voices count > 2 for functions split, chords->voices, voices->chords
%   export-instruments has been redone : do not split MultiMeasureRest and SimultaneousMusic
%   new functions : mmr, seq-r, signatures, keys, marks
%   add-dynamics can add dynamics in a \grace section, using the colon ":" character
%   set-transp allows now a pitch argument
%   make function metronome compatible with lilypond 2.22
%   new : em-with-func, copy-to-with-func, copy-out-with-func, extend apply-to syntax
%   new function : fill-percent. Is build with fill-generic, a func to buid new fill function
%   rename set-notes to set-pitch
%   cp : allows an optional argument keep-last-rests? : (cp [keep-last-rests?] pattern music)
%   str->pos-dyn-list : allows tweaking X alignment
%      "13 ^mf#-0.5" means : at measure 13, -\tweak DynamicText.self-alignment-X #-0.5 ^mf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\include "chordsAndVoices.ly"    %% (original name "chord.ly") See :
                                 %% LSR = http://lsr.di.unimi.it/LSR/Item?u=1&id=761
                                 %% LSR = http://lsr.di.unimi.it/LSR/Item?u=1&id=545
\include "changePitch.ly"        %% LSR = http://lsr.di.unimi.it/LSR/Item?id=654
\include "copyArticulations.ly"  %% LSR = http://lsr.di.unimi.it/LSR/Item?id=769
\include "addAt.ly"              %% http://code.google.com/p/lilypond/issues/detail?id=824
% \include "extractMusic.ly"     %% LSR = http://lsr.di.unimi.it/LSR/Item?id=542
                                 %% extractMusic.ly is already included in addAt.ly
% \include "checkPitch.ly"       %% LSR = http://lsr.di.unimi.it/LSR/Item?id=773
                                 %% checkPitch.ly is already included in chordsAndVoices.ly

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% extract and replace
%% Prepare the \extractMusic and \replaceMusic functions of "extractMusic.ly"
%% to the by-measure-number positionning system. from, to, and start parameters
%% are moments, relative to the beginning of \global which is ZERO-MEMENT.
%% start is typically the moment where the music (passed in params) is beginning.

%%%% extract %%%%
%% extract adds an optional parameter compared to extract-range.
#(define* (extract music from to #:optional (start ZERO-MOMENT))
(parameterize ((*current-moment* start))
  (extract-range (expand-notes-and-chords-copy-of music) ; see changePitch.ly
                 from to)))

%% x-extract is an extended version : used by em-with-func and rm
#(define (x-extract music-or-musics . args)
(if (pair? music-or-musics)
  (map (lambda(x) (apply x-extract x args)) music-or-musics)
  (apply extract music-or-musics args)))

%%%% replace %%%%
%% scheme function corresponding to the \replaceMusic function in extractMusic.ly
#(define* (replace music where replacement-music #:optional (start ZERO-MOMENT))
(let* ((repla (ly:music-deep-copy replacement-music))
       (repla-len (ly:music-length repla))
       (repla-end (ly:moment-add where repla-len))
       (res (filter defined-music? (list
              (extract music start where start)
              ;; (ly:set-origin! repla)  ;; to test
              repla
              (extract music repla-end (ly:music-length global) start)))))
 ;(display repla-len)(display "\n")(display repla-end)
 (if (null? res) (make-music 'Music 'void #t)
                 (reduce-seq (make-sequential-music res)))))

%%%%%%%%%%%% the by-measure-number positionning system %%%%%%%%%%%%%%%%%%%%%%%%%%
%% = defining a musical position using measure numbers. %%
%% A position will be either a measure number alone
%%  (ex 15 => measure 15), or either a list in the form :'(n 2^i 2^j 2^k ...)
%%   with n as the measure number, following by powers of 2 representing a basic
%%   note duration (1 = 2^0 = whole-note, 2 = 2^1 = half-note, etc ..).
%% ex : '(25 2 4) => measure 25 + a half note + a quarter note.
%% The numbers can be negative : '(25 -8) => a eigth before measure 25.
%% The function pos->moment convert a position using this system, into the system
%% used in Lilypond with moments, and moment->pos does the inverse

%% Here is a global variable to start from a measure number different from 1.
%% Is initialized by ... init (see later)
#(define first-measure-number #f)

%%%% conversion position <-> moment utilities %%%%%%

% extractMusic.ly defines a moment->int function. We need here a int->moment
% int->moment deals basically with powers of two : 2 4 8 16...
#(define (int->moment n) ; (better name : scheme->moment ?)
"If n = 0, returns ZERO-MOMENT
If n is an exact integer (positive or negative), returns <moment 1/n>
If n is a decimal number (4.3 for ex), the integer part 4 is the main duration and the
decimal part 3 is the count of . for this duration, and the function returns the length
 of <duration 4...> ie <moment 15/32>
The count of . defaults to 1 if decimal part = 0 ; (int->moment 4.) returns <moment 3/8>
If n is a rationnal, returns <moment n>
Any moment remains unchanged"
(cond              ; Note for guile and integers : (integer? 4.) returns #t !
  ((integer? n) (cond  ; is n an exact or inexact integer ? ie 4 or 4. form ?
     ((= n 0) ZERO-MOMENT) ; n will be a denominator so must be non 0
     ((exact? n)           ; 4 => <mom 1/4>
        (ly:make-moment 1 n 0 0)) ; zeros needed ! ( n can be < 0 )
     (else                 ; 4. => <mom 3/8>
        (ly:make-moment 3 (* 2 (inexact->exact n)) 0 0))))
  ((ly:moment? n) n)      ; this syntax is used by moment->pos below.
  ((rational? n) (if (exact? n) ; 7/8 or 4.3 form ?
     (ly:make-moment n)    ; 7/8 => <mom 7/8>
     (let* ((10n (inexact->exact (truncate (* 10 (abs n))))) ; 4.3 -> 43 | -4.3 -> 43
            (q (quotient 10n 10)) ; => 4 (log2 for duration: 2 (4 = 2^2))
            (r (modulo 10n 10))   ; => 3 (number of points = rest of division)
            (mom (ly:duration-length (ly:make-duration (ly:intlog2 q) r))))
       (if (> n 0) mom (mom-sub ZERO-MOMENT mom)))))
  (else ZERO-MOMENT))) % ignores any error

%% The main code in pos->moment is provided by measure-number->moment, which is
%% defined in "extractMusic.ly". idem for mom-add which allows args count > 2
#(define (pos->moment pos)
"Convert a measure-based position into a moment."
(let* ((pos-list (if (pair? pos) pos (list pos)))
       (mom (let ((n (car pos-list))
                  (m first-measure-number)) ; if n < m, take m later
         (cond ((not (integer? n)) ; can be 'end or 'begin
                  (if (eq? 'end n)(ly:music-length global) ;
                                  ZERO-MOMENT)) ; n = 'begin
               (else (measure-number->moment (max n m) m))))))
  (apply mom-add mom (map int->moment (cdr pos-list)))))

#(define (moment->pos moment)
"Returns a measure-based position from the moment position mom, as a list of 2 elements :
  1st element : the measure number,
  2nd element : the remaining beats in this measure as a moment."
(let loop ((l (or (*timing-sections*)
                  (make-timing-sections first-measure-number))))
  (let* ((section (car l))
         (mom (vector-ref section 1)))
    (if (ly:moment<? moment mom)
      (loop (cdr l))
      (let ((n (vector-ref section 0))
            (1measure-len (vector-ref section 2)))
        (let((number (+ n (mom->integer (mom-delta-div mom moment 1measure-len))))
             (remain (mom-delta-mod mom moment 1measure-len)))
          (if (ly:moment<? 1measure-len ZERO-MOMENT)
            (list number (mom-add remain 1measure-len))
            (list number remain))))))))


%%%%% pos utilities %%%%%%%
%% 2 basics functions pos:num pos:remain
#(define (pos:num pos)
"Returns the measure number of pos (a int)"
(if (pair? pos) (car pos) pos))

#(define (pos:remain pos)
"Returns the remaining beats (a moment)"
(if (pair? pos)
  (apply mom-add (map int->moment (cdr pos)))
  ZERO-MOMENT))

%% pos? is a lazy definition of a "by-measure-number pos" but enough for our purpose
#(define (pos? x)
"Is x a measure-based position ?"
     (define (is-measure-number? x)(or (integer? x)(memq x '(begin end))))
(or (is-measure-number? x)
    (and (pair? x)
         (is-measure-number? (car x)))))

#(define (pos-sub pos2 pos1)
"Returns the length between pos1 and pos2"
(ly:moment-sub (pos->moment pos2) (pos->moment pos1)))

#(define (pos->string pos) ; used by str->pos-dyn-list
(cond ((number? pos)(number->string pos))
      ((pos? pos)(format #f "'~a" pos))  ; must be a list of numbers
      (else (ly:error "~a is not a valid position !" pos))))

%%%% dealing with arguments %%%%
%% Lot of arguments of functions defined in arranger.ly, can have several types.
%% An argument called `obj for example, can be genarally :
%%        a music, a list of musics, a symbol or a list of symbols.
%% Here are some utilities to get informations from these arguments
#(define (obj->music x)
(cond
  ((ly:music? x) x)
  ((symbol? x) (primitive-eval x)) ;; try (module-ref (current-module) x)
  ((pair? x) (map obj->music x))
  ((ly:pitch? x) (make-music 'NoteEvent 'pitch x))
  (else x))) % ly:error ?

#(define (obj->music-list obj)
(if (cheap-list? obj)
  (map obj->music obj)
  (list (obj->music obj))))

#(define (instrument? x)
(and (symbol? x)
     (or (defined? x)
         (begin
           (format #t "Symbol ~a unknown !\n" x)
           #f))))

#(define (obj->instru-list obj)
(filter
  (lambda(x); x should be a symbol or a list of symbols
    (or (and (pair? x)(pair? (filter instrument? x)))
        (instrument? x)))
  (if (cheap-list? obj) obj (list obj))))

#(define (music-obj? obj)
"Is obj a music, or a list with only music elements ?"
(or (ly:music? obj)
    (and (pair? obj) ;; ly:music-list? perhaps ?
         (music-obj? (car obj))
         (let ((next (cdr obj)))
           (or (null? next)
               (music-obj? next))))))

#(define (music-func->obj-func music-func)
   (define (func obj) (if (cheap-list? obj)
     (map func obj)
     (music-func (ly:music-deep-copy (obj->music obj)))))
   func)

%% not-procedure? allows separation slashes / in . args (see x-rm for ex)
%% In guile 2.0 it will be possible to use the new negate function.
#(define (not-procedure? x)(not (procedure? x)))     % (negate procedure?)
#(define (not-string-null? x)(not (string-null? x))) % (negate string-null?)
%%%%  Utilities for making lists. %%%%
%%%% lst %%%%
%% can be use also by user.  example :
%% fls = '(piccolo flI flII)
%% all = (lst fls 'oboe 'bassoon) => '(piccolo flI flII oboe bassoon)
#(define (lst . args)
"Returns args but if an element is a list, his elements are inserted 
in the parent list instead."
(fold-right
  (lambda(x prev-lst)(if (cheap-list? x) ; don't change cheap-list? by pair? !
    (fold-right cons prev-lst x) ; (append x prev-lst)
    (cons x prev-lst)))
  '()
  args))

%% like lst but goes deeper.
#(define (flat-lst . args)
"Returns a big list of elements of args, with no sub-lists."
(fold-right
  (lambda(x prev-lst)(if (cheap-list? x) ; don't change cheap-list? by pair? !
    (fold-right cons prev-lst (apply flat-lst x));(append (apply...) prev-lst)
    (cons x prev-lst)))
  '()
  args))

#(define (lst-diff mainlist . tosubstract)
"Substract instruments of `tosubstract from instruments list `mainlist."
(lset-difference eq? mainlist (flat-lst tosubstract)))

%% zip
% redefine guile zip, to be used by dispatch-chords
% GUILE : (zip '(A1 A2) '(B1 B2 B3)) => '((A1 B1) (A2 B2))
% HERE  : (zip '(A1 A2) '(B1 B2 B3)) => '((A1 B1) (A2 B2) (B3))
% Well, we keep "zip" as name. Can be perhaps dangerous ..., but we
% save the Guile zip
#(define guile-zip zip)
#(define (zip . args)
"Behaves as GUILE zip, but also adds elements from list bigger
than the smaller length list. Lists are proper list (predicate proper-list?)"
;; args = '((A1 ... Ai ...) (B1 ... Bi ...) ... (X1 ... Xi ...) ...)
(let loop ((i-list args) ;; tail of each elt : '((Ai Ai+1 ...)(Bi Bi+1 ...) ...)
           (res '()))         ;;  the zipped list to build
   (let loop2 ((X-list i-list);; tail of i-list '((Xi Xi+1 ...)(Yi Yi+1 ...) ...)
               (zip-elt '())       ;; will be '(Xi...Bi Ai), after adding Xi
               (next-i-list '()))  ;; must be '((Ai+1 Ai+2...)(Bi+1 Bi+2...) ...)
     (if (pair? X-list)
       (let ((x (car X-list))) ;; '(Xi Xi+1 ...)
         (if (pair? x)
           (loop2 (cdr X-list)                ;; '((Yi Yi+1) ...)
                  (cons (car x) zip-elt)      ;; adds Xi
                  (cons (cdr x) next-i-list)) ;; '((Xi+1 Xi+2 ...) ... (Ai+1 Ai+2...))
           (loop2 (cdr X-list)   ;; '((Yi Yi+1) ...)
                  zip-elt        ;; nothing to add
                  next-i-list))) ;; don't add empty list
       (if (pair? zip-elt)
         (loop (reverse next-i-list)(cons (reverse zip-elt) res))
         (reverse res))))))
%{ OLD VERSION of x-pos
%% Making a list of pos from a pattern. To use for ex with x-rm like this :
%% music = { e'2 f' | g' f' | e'1 }
%% (apply x-rm 'music #{ c'8 c' c' #} (x-pos 1 3 '((n 8)(n 2 8)))) ;; use apply
%% => music = { e'8 c' c' c' f' c' c' c' | g' c' c' c' f' c' c' c' | e'1 }
#(define* (x-pos from-measure to-measure #:optional pos-pat (step 1))
"Makes a list of 'by-measure-positions' from the pattern pos-pat.
pos-pat is a quoted list of positions with a letter, typically n, instead of the
measure number. The pattern is expanded replacing n (the letter) by from-measure
and increasing repeatedly this value by step, while lesser to to-measure.
By default, pos-pat is '(n), step is 1"
(let ((func (if pos-pat
              (lambda(n prev)(fold-right cons prev (let loop ((x pos-pat))(cond
                                                     ((symbol? x) n)
                                                     ((pair? x)(map loop x))
                                                     (else x)))))
              cons))
      (d (- to-measure from-measure)))
(fold-right func '() (iota (quotient (+ d (remainder d step)) step) ; count
                           from-measure
                           step))))
%{ #(for-each (lambda(x)(format #t "~a\n" x)) (list    ;; tests
 (x-pos 10 12)               ;; => (10 11)
 (x-pos 10 12 '(n (n 4)))    ;; => (10 (10 4) 11 (11 4))
 (x-pos 10 12 '(n (n 4)) 2)  ;; => (10 (10 4))
 (x-pos 10 13 '(n (n 4)) 2)  ;; => (10 (10 4) 12 (12 4))
 (x-pos 10 14 '(n (n 4)) 2)  ;; => (10 (10 4) 12 (12 4))
 ))  %}
%}

%{ x-pos
syntax 1 :
(x-pos from-measure to-measure [pos-pat [step]])
Makes a list of 'by-measure-positions' from the pattern : pos-pat.
pos-pat is a quoted list of positions with a letter, typically n, instead of the
measure number. The pattern is expanded replacing n (the letter) by from-measure
and increasing repeatedly this value by step, while lesser to to-measure.
By default, pos-pat is '(n), step is 1"

syntax 2 :
((x-pos [pos-pat [step]]) from-measure1 to-measure1 [from-measure2 to-measure2 ...])
x-pos is applied repeatedly with the same pos-pat and step to all couple of from/to measures.
The result lists of each iteration are concatained together.
%}

#(define (x-pos param . args)
   (define* (basic-x-pos from-measure to-measure #:optional pos-pat (step 1))
     ; syntax 1
     (if (eq? from-measure 'begin) (set! from-measure first-measure-number))
     (if (eq? to-measure 'end) (set! to-measure (pos:num (moment->pos (ly:music-length global)))))
     (fold-right
       (if pos-pat
         (lambda(n prev)(fold-right cons prev (let loop ((x pos-pat))
               (cond ((symbol? x) n)
                     ((pair? x)(map loop x))
                     (else x)))))
         cons)
       '()
       (iota           ;; iota param count ↴
         (let ((d (- to-measure from-measure)))
           (if (= step 1) d
                          (quotient (+ d (remainder d step))
                                    step)))
         from-measure  ;; iota param start
         step)))       ;; iota param step
   (define ((ext-x-pos . args) . bar-numbers)
     ; syntax 2
     (let ((l (filter not-procedure? bar-numbers)))
       (let loop ((l l)
                  (i (length l))
                  (res '()))
         (if (< i 2)
          (reverse res)
          (let ((pos-list (apply basic-x-pos (first l) (second l) args)))
            (loop (cddr l)
                  (- i 2)
                  (fold cons res pos-list)))))))
(apply (if (pair? param) ext-x-pos basic-x-pos)
       param ; = pos-pat (a list) syntax 2, or = from-measure syntax 1
       args))

%% Guile 2.0 propose a compose function, not 1.8 :-(
% #(define ((compose func-n . other-funcs) obj)
% "((compose func-n ... func-2 func-1) obj) will result to
%   (func-n ... (func-2 (func-1 obj)))"
% (if (null? other-funcs)
%   (func-n obj)
%   (func-n ((apply compose (car other-funcs)(cdr other-funcs)) obj))))

%%%% skip utilities %%%%
#(define* (moment->skip mom-or-rationnal #:optional text dir X-align Y-offset)
(let ((skip (make-music 'SkipEvent 'duration (make-duration-of-length
              (if (ly:moment? mom-or-rationnal)
                mom-or-rationnal        ; a moment, (ly:make-moment 3/4) for ex
                (ly:make-moment mom-or-rationnal)))))); a rationnal, 3/4 for ex
 (if (and text (markup? text))
   (let ((script (make-music 'TextScriptEvent 'text text)))
      (if (member dir (list UP DOWN))(ly:music-set-property! script 'direction dir))
      (if (number? X-align) (ly:music-set-property! script 'tweaks
                              (list (cons 'self-alignment-X X-align))))
      (if (number? Y-offset)(ly:music-set-property! script 'tweaks
                              (acons 'Y-offset Y-offset (ly:music-property script 'tweaks))))
      (ly:music-set-property! skip 'articulations (list script))))
 skip))

#(define (pos->skip pos)
(make-music 'SkipEvent 'duration
  (make-duration-of-length (pos->moment pos))))

% ex in a 3/4 signature music :
%%  (volta-repeat->skip 15/4 3/4 2/4)  ; 15/4 = 5 measures
% => \repeat volta 2 s4*15 \alternative { s4*3 s2 }
% same result, if these 5 measures are from for ex measure 7 to 12, with  :
%   (volta-repeat->skip (pos-sub 12 7) 3/4 2/4)
#(define (volta-repeat->skip mom . alts)
"Make a repeat volta structure filled with skip musics.
All arguments are moments or directly rationnal numbers as well.
`mom is the length of the main section, and the other arguments in `alts
are the length of each \\alternative section"
  (make-music 'VoltaRepeatedMusic
    'repeat-count (length alts)  ; if alts = '(), repeat-count is ignored.
    'elements (map moment->skip alts) ; \alternate list
    'element (moment->skip mom)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% user functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% the init function %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Before using all the fonctions of arranger.ly, the user *has* to call init
%%  first [ Note that symb-list can be empty : '() ]
#(define* (init symb-list #:optional mes1num)
"Declares and initializes with multiRests all instruments (symbols) of the given list 
and optionally set the number of first measure."
(let*((spaces "\n  ")
      (errormsg (lambda(str)
        (format #t "**** function init error :~a~a\n******\n" spaces str))))
(cond
  ((not (defined? 'global))
      (errormsg (string-append
                  "No \\global variable found !"
                  spaces "Example :"
                  spaces "  global = { s1*20 \\time 5/8 s8*5*10 }")))
  ((not (ly:music? global))
      (errormsg "\\global is not a valid music variable"))
  (else
    (set! first-measure-number (if (and mes1num (integer? mes1num))
                                  mes1num 1))
    (let ((global-end (ly:music-length global)))
      (if (equal? ZERO-MOMENT global-end)
        (errormsg "\\global length must be not null !")  ;; how many mmR to add ?
        (if (pair? symb-list)
          (let* ((rests #{ r\longa #infinite-mmR #}) ; (quasi) infinite length .. (defined in extractMusic.ly)
                 (from (ly:moment-sub (ly:make-moment 4/1) 	              ; the len of r\longa
                                      (pos->moment first-measure-number)))) ; is > 0 if \partial
            (for-each
              (lambda(sym)   ;; adds multiRest to all instruments
                 (if (not (symbol? sym))
                   (errormsg "the init parameter must be a list with only symbol elements ***\n")
                   (ly:parser-define! sym (extract-during rests from global-end))))
                      ;; the method (mmrest-of-length global) doen't work with \partial
              symb-list)
            ; if symb-list is empty, we let the timing-sections list be recalculated at each build-in function call.
            ; (the variable (*timing-sections*) is let to #f (default)). It allows user to add in \global,
            ; some timing events, with whatever functions of arranger.ly (rm, rm-with, x-rm etc...), while keeping,
            ; in "real-time", the right correspondance between measure numbers and moments.
            ; When user calls (or re-calls) init with a not empty symb-list, it means that \global is achieved. We can set
            ; the timing-list in a definitive state.
            (*timing-sections* (make-timing-sections first-measure-number))))))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%% cut and past functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 2 main functions : em and rm

%%%%%%% em %%%%%%%
%% em (= Extract Music) is like the extract function above but use measure positioning
%% The param `obj is assumed to begin at ZERO−MOMENT, like \global does.
%% If it is not the case, you will have to set the additional parameter obj-start-pos
%% For example, (em music 4 5 3) will extract the 2nd measure of music, because music
%% begins measure 3 in the score.
%% A more generic function is defined first. It is used rather internally.
#(define* ((em-with-func func) obj from-pos to-pos #:optional obj-start-pos)
"Extracts music between the 2 by-measure-number positions from-pos and to-pos, and apply func to it.
obj can be a music, an instrument (a parser-defined symbol), a list of music
 or a list of instruments. Returns a list of music if obj is a list, or a music otherwise.
obj-start-pos is the position where obj was designed to begin in the score."
(let ((from (pos->moment from-pos))
      (to (pos->moment to-pos))
      (start (if obj-start-pos (pos->moment obj-start-pos) ZERO-MOMENT)))
   (define (do-extract x) (x-extract (obj->music x) from to start))
(if func
  (cond
    ((pair? func) ;; if func in the form '(sub-func . (list args-obj1 args-obj2 ...) ?
       (let((sub-func (car func))
            (args (cdr func)))
          (map (lambda(arg x)
                 (let ((func (if (pair? arg)        ; func = (sub-func arg)
                               (apply sub-func arg)
                               (sub-func arg)))
                       (extracted (do-extract x)))
                   (if (pair? extracted) (map func extracted) (func extracted))))
               args
               (apply circular-list (obj->music-list obj)))))
    (else
      (let ((extracted (do-extract obj)))
         (if (pair? extracted) (map func extracted) (func extracted)))))
  ;; not func
  (do-extract obj))))

#(define em (em-with-func #f))

%%%%%% rm %%%%%%%
%% rm is probably the main function of "arranger.ly".
%% rm (= replace music) behaves like \ReplaceMusic (see extractMusic.ly) but can
%% deal with several instruments at the same time.
%% Please, note the difference of, for example
%%   (rm music 4 #{ c'1 #}) %% returns a copy of music with measure 4 replaced by a c'1
%%                          %% but music is kept unmodified
%%   (rm 'instru 4 #{ c'1 #}) %% idem but the return value is re-affected to 'instru.

#(define* (rm obj where-pos replacement
                              #:optional repla-extra-pos obj-start-pos)
"Replace in obj, the music beginning at where-pos, by replacement.
obj is a parser-defined symbol, a music, or a list containing itself
either symbols or musics or even lists of symbols or musics.
replacement is a music or a list of music.
A replacement is done element n with element n of each list. (If replacement
length is smaller, his last element is taken at the end).
Returns a list of music if obj is a list, and a music otherwise. Each symbols are
re-defined with the whole new music value.
repla-extra-pos can be used to take only a portion of replacement.
If repla-extra-pos is before where-pos, replacement events in range :
  [repla-extra-pos where-pos[ will *not* be taken. 
If repla-extra-pos is after where-pos, *only* replacement event in range :
  [where-pos repla-extra-pos [ will be taken.
obj-start-pos is the measure where obj was designed to begin in the score."
(let* ((where (pos->moment where-pos))
       ;(replacement (ly:set-origin! replacement))
       (repla-list (if repla-extra-pos
           (let* ((extra (pos->moment repla-extra-pos))
                  (func (if (ly:moment<? where extra)
                    (lambda (repla) ;; extra is like a `to parameter (see em)
                      (x-extract repla
                          ZERO-MOMENT (ly:moment-sub extra where)))
                    (lambda (repla) ;; extra is like a `origin parameter
                      (x-extract repla
                          (ly:moment-sub where extra) (ly:music-length repla))))))
               (map func (obj->music-list replacement)))
           (obj->music-list replacement))) ; if no repla-extra-pos
       (start (if obj-start-pos (pos->moment obj-start-pos) ZERO-MOMENT)))
   (define (do-replace x repla)
      (cond ((pair? x) (if (pair? repla)
               (map do-replace x repla)
               (map (lambda(y)(do-replace y repla)) x)))
            ((ly:music? x) (replace x where repla start))
            ((instrument? x)
               (let ((res (replace (obj->music x) where repla start)))
                 (ly:parser-define! x res)
                 res))
            (else (ly:warning
         "the parameter obj of rm must be a music or a defined symbol"))))
 (if (pair? obj)
  (let ((repla-list (append repla-list (circular-list (last repla-list)))))
    (map do-replace obj repla-list)) ; the last elt of repla-list is on a loop
  (do-replace obj (car repla-list)))))

#(define (x-rm obj replacement pos1 . otherpos)
"Shortcut for (rm obj pos1 replacement)(rm obj pos2 replacement) etc ... but works
also for music or list of musics.
A slash / is allowed to separate pos"
(fold
  (if (music-obj? obj)
    (lambda (pos prev)(rm prev pos replacement))
    (lambda (pos prev)(rm obj pos replacement)))
  obj
  (cons pos1 (filter not-procedure? otherpos))))

#(define (rm-with obj pos replacement . pos-replacements)
"Shortcut for (rm obj pos1 repla1)(rm obj pos2 repla2) etc ...
A slash / is allowed to separate each sections
   (rm-with obj pos1 repla1 / pos2 repla2 / pos3 repla3 ...)
Use (delay repla2), if repla2 use a section modified by section [pos1 repla1]"
(let ((music? (music-obj? obj)))
  (let loop ((res (rm obj pos replacement))
             (args (filter not-procedure? pos-replacements))) ;; del all /
     (if (< (length args) 2)
       res
       (loop
         (let ((arg2 (second args)))
           (if (promise? arg2)(set! arg2 (force arg2)))
           (rm (if music? res obj)(first args) arg2))
         (list-tail args 2))))))

#(define* ((copy-to-with-func func) destination source from-pos to-pos #:key source-start-pos . args)
"Copy source to destination to source from from-pos to to-pos applying the func to the copyied music.
source and destination is a parser-defined symbol (an instrument), a music, or a list
containing itself either instruments or musics, or also lists of either instruments or
musics. Musics in source are assumed to begin at measure defined in init (parameter
mes1num, 1 by def).
You can specify several sections to copy by the following way :
 ((copy-to-with-func func) destination sourceA posA1 posA2 / sourceB posB1 posB2 / etc...)
If sourceX is omitted, the prev source is assumed"
(let ((music? (music-obj? destination)))
          (define (source->dest-copy from-pos to-pos)
            ((em-with-func func) source from-pos to-pos source-start-pos))
  (let loop ((res (rm destination from-pos (source->dest-copy from-pos to-pos)))
             (args (filter not-procedure? ;; del all /
                          (if source-start-pos (cddr args) ;; skip key and key-values
                                               args))))
    (if (< (length args) 2)
      res
      (loop
        (let ((arg1 (first args)))
          (if (not (pos? arg1)) ;; => arg1 is a source
            (begin (set! source arg1)
                   (set! args (cdr args))
                   (set! arg1 (first args))))
          (rm (if music? res destination)
              arg1
              (source->dest-copy arg1 (second args))))
        (list-tail args 2))))))

#(define copy-to (copy-to-with-func #f))

#(define ((copy-out-with-func func) obj from-pos to-pos where-pos . other-where-pos)
"Copy the section [from-pos to-pos] to where-pos, applying func to music.
obj must be a symbol of an instrument, or a list of symbols.
You can copy several where-pos :
 (copy-out obj from-pos to-pos where-pos1 where-pos2 where-pos3 ...etc)"
(let ((obj (if (cheap-list? obj) (flat-lst obj) obj))) ; no list inside list - symbols only
         (define (obj->dest-copy from-pos to-pos)
             ((em-with-func func) obj from-pos to-pos))
  (apply x-rm obj (obj->dest-copy from-pos to-pos) where-pos other-where-pos)))

#(define copy-out (copy-out-with-func #f))

% apply-to and x-apply-to must be used with functions with a Music as unique
% parameter such as : set-transp, set-pat, set-arti, set-reverse, set-del-events,
% set-notes+ (see later). `apply-to provide an optional parameter for music
% variables, not beginning at the beginning of the score

#(define* (apply-to obj func from-pos to-pos #:optional obj-start-pos)
"Apply func to obj from `from-pos to `to-pos.
func is a function taking a music parameter : (func music). 
If func is defined by a sub function ((sub-func args) music), user can 
specifies special args for each instrument of obj, by setting func as a pair :
   (sub-func . (list args-instrument1 args-instrument2 ...))
with args-instrumentX either a single argument or a list of arguments for sub-function
obj is a music, a symbol or a list of either musics and symbols.
obj-start-pos is the measure of the score corresponding to the beginning of obj."
 (rm obj from-pos ((em-with-func func) obj from-pos to-pos)
                   #f obj-start-pos))

#(define* (x-apply-to obj func from-pos to-pos #:key obj-start-pos . other-from-to-pos)
"syntax : (x-apply-to obj func from1 to1 / from2 to2 / etc ...
Shortcut for : (apply-to obj func from1 to1)(apply-to obj func from2 to2) etc ..."
(let ((music? (music-obj? obj)))
  (let loop ((res (apply-to obj func from-pos to-pos obj-start-pos))
             (args (filter not-procedure? ;; del all /
                           (if obj-start-pos (cddr other-from-to-pos) ; skip key and his value
                                             other-from-to-pos))))
     (if (< (length args) 2)
       res
       (loop
         (apply-to (if music? res obj) func (first args) (second args) obj-start-pos)
         (list-tail args 2))))))

% to-set-func is a way to quickly transform a function dealing with music events to a
% apply-to and x-apply-to compatible function. Example :
%{ (apply-to 'vl (to-set-func (lambda(m)
                        (if (equal? (ly:music-property m 'pitch #f) #{ c' #})
                          (ly:music-set-property! m 'pitch #{ d' #}))))
                    10 15)
=> all c' will be transformed in d' between measures 10 and 15
%}
#(define (to-set-func func)
   (lambda(music)
     (music-map (lambda(m)(func m) m)
                (ly:music-deep-copy music))))

#(define (xchg-music obj1 obj2 from-pos to-pos . other-from-to-pos)
"Exchange the music of the range [from-pos to-pos[ between obj1 and obj2"
   (define (sub-list->sym obj) ; Take the first not-list element of sub-lists
     (if (cheap-list? obj)
       (map (lambda(x)
              (let loop ((x x))
                 (if (cheap-list? x) (loop (car x)) x)))
            obj)
       obj))
(let loop ((from-pos from-pos)
           (to-pos to-pos)
           (args (filter not-procedure? other-from-to-pos)))
  (let* ((m1 (em (sub-list->sym obj1) from-pos to-pos))
         (m2 (em (sub-list->sym obj2) from-pos to-pos))
         (res (cons (rm obj1 from-pos m2)
                    (rm obj2 from-pos m1))))
    (if (< (length args) 2)
       res
       (begin
         (if (music-obj? obj1) (set! obj1 (car res)))
         (if (music-obj? obj2) (set! obj2 (cdr res)))
         (loop (first args)(second args)(list-tail args 2)))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% shortcuts %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% function rel is basically a shortcut for \relative. But an extension of syntax are provided
% for functions in the form : (func n music), ie for rel, octave and octave+.
% A generic function is first created,

#(define ((int-music-generic func) n . args) ; for func with (func n m) form
   (define (not-integer? m)(not (integer? m)))
   (define (pred->list pred?) ; separate args elts (args is in reverse order)
     (let loop ((l '())
                (entry '())
                (args args))
          (define (add-entry) ; all consecutive elts with pred? = #t go to the same list
            (cond ((null? entry) l)
                  ((null? (cdr entry)) (cons (car entry) l))
                  (else (cons entry l))))
       (if (null? args)
         (add-entry) ; add the last entry and return
         (let ((x (car args)))
           (if (pred? x)
             (loop l (cons x entry) (cdr args))
             (loop (add-entry) '() (cdr args)))))))
(set! args (reverse
  (let ((args (apply lst args)))
    (if (integer? n) (cons n args)  ; n is optional, default it to 0
                     (cons 0 (cons n args))))))
(let((n-list (pred->list integer?))
     (m-list (pred->list not-integer?))) ; music, music-list, symbols, sym list, sub-music list
  ;(format #t "-> n-list : ~a\n-> m-list :\n" n-list) (for-each display-lily-music m-list)
  (let ((res (apply lst
          (map (lambda(n m)
                 (if (pair? n)
                   (if (pair? m) (map func n m)
                                 (map func n (circular-list m)))
                   (if (pair? m) (map func (circular-list n) m)
                                 (func n m))))
               n-list
               m-list))))
    (cond
     ((null? res)(make-music 'Music))
     ((null? (cdr res))(car res))
     (else res)))))

% function rel (shortcut for relative)
% The code #{ \relative c' $music #} is not compatible with all language
% so (ly:make-music-relative! music pitch) is used instead.

#(define (rel n m)
"Make music (or list of musics) m, relative to the central pitch #{ c' #}
transposed by n octaves."
(if (pair? m)
  (map (lambda(x)(rel n x)) m)
  (let ((m (ly:music-deep-copy (obj->music m)))
        (p (ly:make-pitch n 0 0))) ; n = 0  => c'
    (ly:make-music-relative! m p)
    (make-music 'RelativeOctaveMusic 'element m))))

% extends syntaxe. For octave and octave+ see further
#(define rel (int-music-generic rel))

#(define (seq . args)
"Equivalent to { music1 music2 music3 ...}"
(reduce-seq (make-sequential-music (map obj->music (flat-lst args)))))

#(define (sim . args)
"Equivalent to << music1 music2 music3 ...>>"
(make-simultaneous-music (map obj->music (flat-lst args))))

#(define (split arg music1 . musics)
"Syntax : (split ['(id1 id2 id3...)] music1 music2 music3...)
Equivalent to : \\voices id1,id2,id3 ... << music1 \\\\ music2 \\\\ music3 ... >> 
The list of the ids of each voice is optional. The default list is based on the
model '(1 3 5 ... 6 4 2)"
   (define (odd-even-iota n)
     "(makes a list '(1 3 5 ... 6 4 2)"
     (receive (evens odds)
       (partition even? (iota n 1))
       (lst odds (reverse evens))))
(if (null? musics) ; arg must be a music.
  (sim arg (make-music 'VoiceSeparator) music1) ; don't use voicify-music
  (let ((ids (or (and (number-list? arg) arg)   ; needed by voicify-music
                 (and (number? arg) (odd-even-iota arg))))
        (musics (cons music1 musics)))
    (if (not ids) ; no ids specified
      (begin      ; arg must be a music.
        (set! musics (cons arg musics))
        (set! ids (odd-even-iota (length musics)))))
    (let* ((musics-rev (reverse musics))
           (vocify-args (fold
             (lambda(m prev)
               (cons m (cons (make-music 'VoiceSeparator) prev)))
             (list (car musics-rev))
             (cdr musics-rev))))
      (voicify-music (sim vocify-args) ids)))))

#(define (mmr arg0 . args)
"Returns a multiMeasasureRest. Takes 1 rational or 2 bar numbers:
(mmr arg0) has the length of (ly:make-moment arg0)
(mmr arg0 arg1) has the length of the music between bars arg0 arg1"
(make-music 'MultiMeasureRestMusic 'duration
  (make-duration-of-length (if (null? args)
    (ly:make-moment arg0)
    (pos-sub (car args) arg0)))))

#(define (seq-r . args)  ; 2 4 8... or 4. (1 point), 4.2 (2 points), 4.3 (3 points), or a music
"Make a sequential music of rests. 
args is basically a list of powers of two: (seq-r 2 4 8) means { r2 r4 r8 }
If a point . following by a digit 1..9 is appened to the number, this digit will be the points count.
ex : (seq-r 2.3 8) means: { r2... r8 }
No digits after the point defaults count to 1: (seq-r 4.) and (seq-r 4.1) mean both r4.
Any music argument is added unaltered into the sequence"
  (define (number->rest n) ; n = 4 or 4. or 4.2 or 3/8 ... or a music
    (cond ((number? n) (make-music 'RestEvent 'duration
             (if (exact? n)     ;  integer or rational p/q
               (if (integer? n)
                 (ly:make-duration (if (> n 0) (ly:intlog2 n) (1- n))) ; 0 breve -1 longa -2 maxima
                 (moment->rhythm (moment n))) ; will fail if n < 0
               (let* ((10n (inexact->exact (truncate (* 10 (abs n))))) ; 4.3 -> 43
                      (q (quotient 10n 10)) ; 4 : for the log of duration
                      (r (modulo 10n 10)))  ; 3 : number of points = rest of the division
                 (ly:make-duration (if (> n 0) (ly:intlog2 q) (1- q)) (if (= r 0) 1 r))))))
          (else (obj->music n)))) ; Is obj->music needed ?
(reduce-seq (make-sequential-music (map number->rest (flat-lst args)))))

#(define (at pos mus)
   (if (cheap-list? mus)(map (lambda(x)(at pos x)) mus)
                        (seq (pos->skip pos) mus)))

#(define* ((sym-append sym2 #:optional to-begin?) sym1)
(cond ((symbol? sym1)(if to-begin? (symbol-append sym2 sym1)
                                   (symbol-append sym1 sym2)))
      ((pair? sym1)(map (sym-append sym2 to-begin?) sym1))
      (else (ly:error "Arguments for sym-append must be symbols\n"))))

#(define* (def! sym #:optional music)
"Equivalent to a declaration at the top-level. sym is a symbol or a list of
symbols and the value associated for each of them, is either music, if specified,
either by default, a skip of the length of global."
(for-each (lambda(x y)(if (symbol? x)
            (ly:parser-define! x
               (if y (ly:music-deep-copy (obj->music y))
                     (moment->skip (ly:music-length global))))
            (ly:warning "def! needs a symbol or a list of symbols as parameter !\n")))
          (if (pair? sym) sym (list sym)) ;; x
          (cond                           ;; y
             ((not music)(circular-list #f))
             ((pair? music)
                 (append music (circular-list (last music))))
             (else (circular-list music)))))

#(define (x-em obj . from-to-pos)
"(x-em pos1 pos2 / pos3 pos4 / ...) make the list :
 (list (em obj pos1 pos2) (em obj pos3 pos4) ...)"
(let ((args (filter not-procedure? from-to-pos)))
  (let loop ((l args)
             (i (length args))
             (res '()))
    (if (< i 2)
      (reverse res)
      (loop (cddr l)
            (- i 2)
            (cons (em obj (first l) (second l)) res))))))

#(define (cut-end obj new-end-pos . args)  ;; args = start-pos (see em)
   (def! obj (apply em obj (moment->pos ZERO-MOMENT) new-end-pos args)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% transposition %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% examples of use set-transp
% -> defining transpositions function :
% ex : all ='(vlI vlII vle vlc) ; etc ...
%      #(let ((5th (set-transp 0 4 0)))
%         (apply-to all 5th 23 45))
% will transpose to the dominante the entire section [23 45].
% -> To transpose a melody in c\major to a\minor, use :
% (set-transp (lambda(p)
%   (ly:make-pitch 0 -2 ; (3rd minor for c d f g b and 3rd major for e a (notename 2 5)
%     (if (member (ly:pitch-notename p) '(2 5)) -1/2 0))))

#(define ((set-transp . args) obj . obj-args) ; can be used with apply-to
"
Syntax 1 : (set-transp o n a/2) (o n a as integers, positive or negative)
Syntax 2 : (set-transp p) (p as a pitch)     
Syntax 3 : (set-transp func(p))  (p as the current source pitch)
Apply ly:pitch-transpose to each pitch of `obj. The delta-pitch is:
either the pitch o octaves, notename n, a/2 alterations (syntax 1),
either p (syntax 2), or either the pitch returned by the callback 
function func (syntax 3).
`obj can be an instrument (a symbol), a list of instruments, a music or a
list of musics.
If `obj is a list, or if other arguments are given in obj-arg, the function
returns a flat list of transposed music."
(define delta #f)
(case (length args)
  ((1)(let ((arg (car args)))
        (cond ((procedure? arg) (set! delta arg))
              ((ly:pitch? arg) (set! delta (lambda(dummy) arg))))))
  ((3)(if (every (lambda(r)(or (integer? r)(rational? r))) args) ;(number-list? args)
        (set! delta (lambda(dummy)(apply ly:make-pitch args))))))
(if (not delta) (ly:error "set-transp bad arguments : ~a" args))
(let ((musics (map expand-notes-and-chords-copy-of ; see changePitch.ly
                   (obj->music-list                ; returns a list of musics
                      (flat-lst obj obj-args))))
      (delta-transp (lambda(m)
        (let ((p (ly:music-property m 'pitch #f)))
          (if p (ly:music-set-property! m 'pitch
                  (ly:pitch-transpose p (delta p))))
          m))))
  (for-each
    (lambda(music)(music-map delta-transp music))
    musics)
  (if (null? (cdr musics)) (car musics) musics)))

%%%
% As I use a lot (set-transp n 0 0), I have added 2 functions : octave,
% and octavize. See also octave+ et add-note-octave
#(define (octave n obj)
"Short-cut for ((set-transp n 0 0) obj)"
(if (pair? obj)
  (map (lambda(x)(octave n x)) obj)
  (if (eq? n 0)
    (obj->music obj)
    ((set-transp n 0 0) (obj->music obj)))))

#(define ((set-octave n) obj)
   (octave n obj))

#(define octave (int-music-generic octave)) % syntax extension

#(define (octavize n obj from-pos to-pos . pos-args)
"Transpose the section [from-pos to-pos] by n octaves.
You can specify several sections by the following way :
 (octavize n obj pos1 pos2 / pos3 pos4 / pos5 pos6 etc...)"
(apply x-apply-to obj (set-transp n 0 0) from-pos to-pos pos-args))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% voices %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%#(define (voice n music)
%"Extract the n-th Voice in a multiple Voices music"
%(extract-voice (ly:music-deep-copy music) n #f))

#(define (voice n . args)
"Syntax : (voice n [m p ...] music)
Extracts the n-th voice in a multiple voices music.
If other numbers m, p ... are given, returns a list of all matching voices.
If a number is greater than the number of voices, the last voice is returned."
(let* ((args (reverse (cons n args)))
       (music (car args))
       (n-list (cdr args)))  ; in reversed order
  (if (and (ly:music? music)
           (pair? n-list))
    (let ((res (fold (lambda(n prev) (cons
                       (extract-voice (ly:music-deep-copy music) n #f)
                       prev))
                     '()
                     n-list)))
      (if (and (pair? res)(null? (cdr res)))
        (car res)
        res))
    (ly:error "Bad syntax for procedure voice.\n\t (voice n [m p ...] music)"))))

#(define ((set-voice n . args) music)
   (apply voice n (lst args music)))

#(define (replace-voice n music repla)
(map-some-music
   (lambda(x)
     (case (ly:music-property x 'name)
       ((SimultaneousMusic)
         (let loop ((es (ly:music-property x 'elements))
                    (i 1)
                    (res '()))
           (cond
             ((null? es)(ly:music-set-property! x 'elements (reverse res))
                        x)
             ((eq? (ly:music-property (car es) 'name) 'VoiceSeparator)
                (loop (cdr es) i (cons (car es) res)))
             ((= i n)(loop (cdr es) (1+ i) (cons repla res)))
             (else (loop (cdr es)(1+ i)(cons (car es) res))))))
       ((EventChord) x)
       (else (and (ly:music-property x 'duration #f) x))))
   (ly:music-deep-copy music)))

#(define ((set-replace-voice n repla) music)
    (replace-voice n music repla))

%% a generic function for adding voices
#(define* ((add-voice nthvoice combine-func) obj where-pos voice
                    #:optional voice-start-pos to-pos obj-start-pos)
"Combine voice with obj at where-pos.
obj is a symbol of an instrument or a list of symbols.
voice is a music or a list of musics.
Use voice-start-pos, if voice begins before where-pos.
Use to-pos if you want to stop before the end of voice.
Use obj-start-pos if obj doesn't begin at the beginning of the whole music.
combine-func is a function with 2 music parameters (like split or sim) :
  if nthvoice is 1, param1 = voice, param2 = obj
  otherwise param1 = obj, param2 = voice "
(let*((end-pos (or to-pos 'end)) ;
      (voices (if (cheap-list? voice) voice (list voice)))
      (corrected-voices
         (let ((res (if voice-start-pos  ; voice-start-pos < where-pos
                  (em voices where-pos end-pos voice-start-pos)
                  voices)))
            (append res (circular-list (last res)))))
      (musics (em (obj->music-list obj) where-pos end-pos obj-start-pos))
      (corrected-musics (if to-pos
         musics   ; do nothing if to-pos is specified
         (map     ; musics are shortened to the end of voices
           (lambda (m v)(extract-during m ZERO-MOMENT (ly:music-length v)))
           musics corrected-voices)))
      (replas (if (= nthvoice 1)
         (map combine-func corrected-voices corrected-musics)
         (map combine-func corrected-musics corrected-voices))))
 (rm obj where-pos replas)))
%%%%%%%%%%%%%%%%%%% derivated functions %%%%%%%%%%%%%%%%%%%
%%%  add-voice2, add-voice1 %%%
%      syntaxe : (add-voice1 obj where-pos new-voice
%                    #:optional voice-start-pos to-pos obj-start-pos)
%       => In instrument obj (a symbol), at measure where-pos, replace existing
%       music by << [existing music] \\ new-voice >> for add-voice2
%          or by << new-voice \\ [existing music] >> for add-voice1
%     See add-voice above for the meaning of optional arguments
#(define add-voice1 (add-voice 1 split))
#(define add-voice2 (add-voice 2 split))

%%%  merge-in %%%
%       (merge-in obj where-pos new-voice
%                #:optional voice-start-pos to-pos obj-start-pos)
%         => << new-voice [existing music] >>
#(define merge-in (add-voice 2 sim))

#(define (merge-in-with obj pos voice . pos-musics)
(let loop ((pos pos)
           (music voice)
           (args (filter not-procedure? pos-musics)))
  (let ((res (merge-in obj pos music)))
    (if (>= (length args) 2)
      (loop (first args)(second args)(list-tail args 2))
      res))))

%% derivated from \partCombine
% #(define (part-combine part1 part2)
% ;(make-part-combine-music (list part1 part2) #f)) ;; <- lilypond 2.18
% ; new in lilypond 2.20 : see ly/music-functions-init.ly ;
% (make-directed-part-combine-music #f '(0 . 8) part1 part2
%     #{ \with { \voiceOne \override DynamicLineSpanner.direction = #UP } #}
%     #{ \with { \voiceTwo \override DynamicLineSpanner.direction = #DOWN } #}
%     #{ #}))

#(define part-combine partCombine)

#(define combine1 (add-voice 1 part-combine))

#(define combine2 (add-voice 2 part-combine))

%%%  add-voice-octave %%%  (see also add-note-octave)
#(define (add-voice-octave n sym from-pos to-pos . args)
"Double by n octave,the music in instrument sym, in the range [from-pos to-pos].
A serie of ranges can be specified like in this example :
(add-voice-octave -1 'bassoon 5 7 / 9 '(10 2) / '(13 2 4) 15)"
(let ((new-voice (octave n (em sym from-pos to-pos)))
      (nvoice (if (> n 0) 1 2)))
  ((add-voice nvoice sim) sym from-pos new-voice)
  (if (pair? args)(begin
    (set! args (filter not-procedure? args)); del the /
    (if (>= (length args) 2)
       (apply add-voice-octave n sym (first args)(second args)(list-tail args 2)))))))
%#(define add-octave add-voice-octave) % for compatibility with my old works

#(define (dispatch-voices instruments where-pos music-with-voices . args)
"At measure where-pos, assign to the first element of instruments the first
voice of music-with-voices, the second one to the second voice, and so on."
(let ((i 1))
  (for-each
    (lambda (x)
      (for-each
        (lambda (sym)
            (apply rm sym where-pos (voice i music-with-voices) args))
        (obj->instru-list x))
      (set! i (1+ i)))
    instruments)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% chords %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define (note . args)
"Syntax : (note n [m p ...] music)
Extracts the n-th note of each chords in music, keeping articulations.
If other numbers m, p ... are given, returns chords with all matching notes
If no note matches, returns the last note of the chord."
(let ((music (and (pair? args)(pair? (cdr args))
                  (obj->music (last args)))))
  (if (and music (ly:music? music))   ;; see chordsAndVoices.ly for extract-note
    (apply extract-note (ly:music-deep-copy music) args) ; (args is filtered)
    (ly:error "Bad syntax for procedure note.\n\t (note n [m p ...] music)"))))

#(define ((set-note . args) music)
(apply extract-note (ly:music-deep-copy music) args))
% a function set-pitch is defined further (for changing pitch of notes)

% set-notes+ builds chords by adding new notes to the existant one.
#(define ((set-notes+ . args) music) ; args = music1 music2 etc...
"((set-notes+ music1 music2 ...) music) transforms the first note of music, in a 
chord build with this note and the first note of music1, the first 
note of music2..., then repeat the process with each following notes of each musics."
(let loop ((music (ly:music-deep-copy music))
           (args args))
  (if (null? args)
    music
    (loop (add-note music (car args)) ;; add-note : see chordsAndVoices.ly
          (cdr args)))))

#(define (notes+ musics . args)
(let ((all-args (flat-lst musics args)))
  (if (null? all-args)
    (ly:error "notes+ needs a least one music argument, or a list of musics")
    ((apply set-notes+ (cdr all-args)) (car all-args)))))

#(define (add-notes obj where-pos music . args) ; args = music1 music2 ...
"(add-notes obj where-pos music [music1 music2 ..[obj-start-pos]])
Apply set-notes+ to obj at pos where-pos, with music music1 music2... 
as sequences of notes to be added to each chords of obj. 
obj-start-pos can be set in last arguments of args"
(if (and (pair? music)(pair? obj))
  (map
    (lambda(instru mus)(apply add-notes instru where-pos mus args))
    obj
    music)
  (let ((obj-start-pos (and (pair? args) (last args)))
        (musics (map ly:music-deep-copy (filter ly:music? (cons music args)))))
    (apply-to obj (apply set-notes+ musics) ;; func
                  where-pos 'end (and (pos? obj-start-pos) obj-start-pos)))))

% voices->chords behaves as \partCombine with \partCombineChords option
% To use with apply-to
%#(define (voices->chords music)
%"Transformes 2 simultaneous voices { a b } { c d } in { <a c> <b d> }"
%((set-notes+ (voice 2 music))(voice 1 music)))

#(define (voices->chords arg . args)
"Syntax : (voices->chords [n] music)
Replaces all simultaneous musics of music by a sequential musics with n notes chords. 
If omitted, n defaults to 2.
The first note of a chord matchs to the last voice but notes order
in chords can be customized by setting n as a list of numbers.
ex : music = << { e' g' } { c' d' } { a b } >>
(voices->chords 3 music) and (voices->chords '(3 2 1) music) result both in
{ <a c' e'> <b d' g'> } but (voices->chords '(2 1 3) music) results to 
{ <c' e' a> <d' g' b> }"
;; set-notes+ used in the loop below are building chords in reverse order !
;; so the list of integers n-list must be reversed.
(let* ((nlist+music (if (ly:music? arg)
         (cons '(1 2) arg) ; ie '(2 1)
         (let ((music (and (pair? args) (car args))))
           (if (not (ly:music? music))
             (ly:error "The second argument of voices->chords must be a music"))
           (cons (cond
                   ((integer? arg) (if (> arg 2) (iota arg 1) '(1 2)))
                   ((and (cheap-list? arg)(pair? (cdr arg))
                         (every (lambda(n)(and (integer? n)(> n 0))) arg))
                      (reverse arg))
                   (else (ly:error
 "The first argument of voices->chords must be an integer, or a list of intergers")))
                 music))))
       (nlist (car nlist+music))
       (music (cdr nlist+music)))
  ;(display-scheme-music (voice (car nlist) music))
  (let loop ((res (voice (car nlist) music))
             (nlist (cdr nlist)))
    (if (null? nlist)
      res
      (loop ((set-notes+ (voice (car nlist) music)) res)
            (cdr nlist))))))

#(define (chords->nmusics n music)
"{<a c' e'> <b d' g'> <c' e' g' c'>} and n=3 results in the list 
{e' g' g'} {c' d' e'} {a b c'}"
(if (> n 0)
  (map (lambda(i)(note i music))
       (iota n n -1)) ; '(5 4 3 2 1) for n = 5
  music))
#(define ((set-chords->nmusics n) music)
 (chords->nmusics n music))

%#(define (chords->voices music)
%"Split 2 notes chords in 2 voices.
%    <a c> <b d> < c e> becomes << { a b c} \\\\ { c d e } >> "
%(split (note 2 music)(note 1 music)))

% chords->voices uses the functions chords->nmusics and the function split
#(define (chords->voices arg . args)
"Syntax : (chords->voices [n] music)
Equivalent to : 
  (split (note n music) (note (- n 1) music) ... (note 1 music))
n = 2 by default.
n is converted by the function split in a list of ids for each voices,
but a list of numbers can be set directly."
(cond
  ((ly:music? arg) (apply split (chords->nmusics 2 arg)))
  ((number? arg) (apply split (apply chords->nmusics arg args)))
  ((number-list? arg) (apply split arg
                                  (apply chords->nmusics (length arg) args)))
  (else (ly:error "syntax error :\n\t(chords->voices [n] music) or (chords->voices '(1 3 ... 4 2) music)"))))

#(define (octave+ n music)
(cond ((= n 0) music)
      ((pair? music)(map (lambda(m)(octave+ n m)) music))
      (else    ; ↱ force resolution pitch of { c4 2 8 } for ex
(let((music (expand-notes-and-chords (obj->music music))))
  (add-note-basic ; see chordsAndVoices.ly : like add-note but without calling expand-notes-and-chords
    music
    (map-some-music
      (lambda(x)
        (and
          (eq? (name-of x) 'NoteEvent) ; we don't want articulations 2 times...
          (make-music 'NoteEvent ;... so make a simple note with a new pitch.
            'duration (ly:make-duration 2 0 1) ; dummy value (ignored by add-note)
            'pitch (ly:pitch-transpose
                     (ly:music-property x 'pitch)
                     (ly:make-pitch n 0 0))
            'force-accidental
                   (ly:music-property x 'force-accidental #f))))
      (ly:music-deep-copy music)))))))

#(define ((set-octave+ n) music)
   (octave+ n music))

% extend the syntax of octave+
#(define octave+ (int-music-generic octave+))

#(define (add-note-octave n obj from-pos to-pos . args)
(apply x-apply-to obj (set-octave+ n) from-pos to-pos args))

% #(define* (reverse-chords n music #:optional (strict-comp? #f))
% "Reverse n times each chord in `music, upward if n positive downward otherwise.
% The lowest (highest) note is transposed to be higher (lower) or equal to the highest (lowest) note.
% If you want a strict compraraison (the 2 notes not equal), set strict-comp? to #t"
% => see now chordsAndVoices.ly

#(define* ((set-reverse n #:optional (strict-comp? #f)) music)
"idem reverse-chords but compatible with apply-to"
(reverse-chords n music strict-comp?))

#(define (dispatch-chords instruments where-pos music-with-chords . args)
"At measure `where-pos, assign to the first element of `instruments the last
note of each chord in `music-with-chords, the second one to the second to last
note, and so on."
(let ((i (length instruments))
      (music (obj->music music-with-chords)))
  (map
    (lambda (instru i)
       (apply rm instru where-pos (note i music) args))
    instruments
    (iota i i -1) ; (iota count start step) : '(i i-1 ... 1)
    )))

%% redefine braketify-chords to accept object argument instead of music
%% (see "copyArticulations.ly")
#(define braketify-chords (music-func->obj-func braketify-chords))

                        %%% chords and voice %%%
#(define (treble-of music) (note 1000 (voice 1 music)))
#(define (bass-of music) (note 1 (voice 1000 music)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% adding texts and quotes %%%%%%%%%%%%%%%%%%%%%%%
#(define (txt text . args)
"syntaxe : (txt text [dir [X-align [Y-offset]]])
equivalent to the sequence :
 \\once \\override Voice.TextScript.self-alignment-X = $X-align 
 \\once \\override Voice.TextScript.Y-offset = $Y-offset
 s1*0^text or s1*0_text or s1*0-text (for dir = UP, DOWN, 0)"
(apply moment->skip ZERO-MOMENT (cons text args)))


#(define adef-size -3)
adefSet = { \set Voice.fontSize = #adef-size
            \override Voice.Stem.length-fraction = #(magstep adef-size) }
adefUnSet = { \unset Voice.fontSize
              \revert Voice.Stem.length-fraction }
#(define (adef music . args)
"Add music in a small size font. A text can be added, as in function txt.
syntaxe : (adef music text dir X-align Y-offset)"
#{ \context Voice
  <<
    $music
    {
      \adefSet
      $(apply moment->skip                          ; set length a very little
            (ly:moment-sub (ly:music-length music)  ; shorter than `music.
                           (ly:make-moment 1/65536)); 1/2^16 whole-note
            args)
      \adefUnSet % will not be erased when some music
    }            % will be added just after `music
  >>
#})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% adding dynamics %%%%%%%%%%%%%%%%%%%%%%%
%%%%%% About pos-dyn-str %%%%%%
%% A pos-dyn-str is a string in the form of :
%% "1 p / (5 8 16) cresc / (8 -16) f > / 10 !"   So :
%%  - a position as defined line 80 (the prime symbol ' before a list '(5 8 16) is optional  )
%%  - 1 or 2 dynamics
%%  - a slash / as separating character
%% X and Y adjustement for dynamics can be achieved by appending to the dynamic :
%%   #alignement-X-val#Y-offset-val or ##X-offset-val#Y-offset-val
%%  f#1#-1.5 results as :
%%  "-\tweak self-alignment-X 1 -\tweak extra-offset #'(0 . -1.5) -\f"
%%  f#1#2#-1.5 results as :
%%  "-\tweak self-alignment-X 1 -\tweak extra-offset #'(2 . -1.5) -\f "
%%  if self-alignment-X not needed : f##-1.5 or f##2#-1.5

% Util function for add-dynamics
#(define (str->pos-dyn-list pos-dyn-str)
"Parse the string pos-dyn-str and return a list of string pair representing a position
and a music with dynamics.
Two specials caracters : the sharp # character for tweaking dynamic positions and the colon : character
to insert the dynamics into a \\grace {} section."
  (define (split-by-space str) (filter not-string-null? (string-split str #\space)))
  (define (split-by-slash str) (map string-trim-both (string-split str #\/)))
  (define (str->pos->pos-str s) (pos->string (eval-string s)))
  (define (pos-str->mom str) (eval-string (string-append "(pos->moment " str ")")))
  (define (sharp->tweaks str) ; checks for # characters and transforms it into dynamic tweaks
    (fold
      (lambda(s prev-s) ; s in the form of: mf or ^mf or mf#0.5 or mf#0.5#-2 or mf#0.5#1#-2
        (let* ((splitted-s (string-split s #\#))    ; #\# : split character = #
               (safe-list (append splitted-s (circular-list #f)))
               (1st (first safe-list))  ; mf or ^mf
               (2nd (second safe-list)) ; 0.5...or #f
               (3rd (third safe-list))  ; -2 (if 4th=#f) or 1 or #f
               (4th (fourth safe-list)) ; -2 or #f
               (dynstr (let ((s2 (substring 1st 0 1)))  ; s2 = string with 1st char
                 (cond ((string=? s2 ":") 1st)  ; will be a skip with no dynamics
                       ((string-contains "-^_" s2)
                          (string-append s2 "\\" (substring 1st 1))) ; => ^\mf
                       (else (string-append  "\\" 1st)))))            ; => \mf
               (align-X-tweak
                 (if (and 2nd (not (string-null? 2nd)))
                   (string-append "-\\tweak self-alignment-X " 2nd) ""))
               (offset-tweak (let ((Y-offset (or 4th 3rd))
                                   (X-offset (or (and 4th 3rd) "0")))
                 (if Y-offset
                   (begin (if (string-null? Y-offset) (set! Y-offset "0"))
                          (string-append "-\\tweak extra-offset #'(" X-offset " . "  Y-offset ")"))
                   "")))
               (final-str (string-join (split-by-space (string-append ; delete all unneeded spaces
                             align-X-tweak " " offset-tweak " " dynstr)))))
          ;(format #t "~a => ~a\n" s final-str)
          (string-append prev-s " " final-str)))
      "" (split-by-space str))) ; (split-by space " mf  cresc ") => '("mf" "cresc")
  (define (colon->music-str str) ; checks for : char and transforms it into "\grace { skip }"
    (define after-grace-frac (scale->factor (ly:parser-lookup 'afterGraceFraction)))
    (let ((len (string-length str)) ; str must be formated : only 1 space between 2 words
          (after-grace #f)) ; actived by "::" ex "p::4 :8 f:16" => \afterGrace s4\p { s8 s16\f }
      (let loop ((grace "")
                 (pos 0))   ; position of the section to parse
        (let ((i (and (< pos len) (string-index str #\: pos))))   ; i = index of semi-column : char
          ; (format #t "\n\tstr = ~s\n\tsemi-column index = ~a" str i )
          (if i                                        ; for str = "\mf:16 :16*2" => i=3 then 7
            (let* ((k (or (string-index str #\space i) len))                  ;  => k=6 then 12
                   (j (1+ i))
                   (2cols? (string-index str #\: j (1+ j)))     ; "::" ? (Returns #f or j)
                   (dur (substring str (if 2cols? (1+ j) j) k)) ; string after ":" or "::"
                   (dyn (substring str pos i))                  ; string before 1st : char
                   (skip (string-append "s" dur                 ; s16 then s16*2
                                            dyn)))              ; \mf then ""
              ; (format #t "\n\tj = ~a\n\tk = ~a\n\t2cols? = ~a\n" j k 2cols?)
              (cond
                (2cols? (let* ((splitted-dur (string-split dur #\:)) ; ex  f::4:15:16 => '("4" "15" "16")
                               (dur (car splitted-dur))			     ; "4"
                               (fracs (cdr splitted-dur))			 ; '("15" "16")
                               (frac (if (null? fracs)
                                 after-grace-frac                    ; afterGraceFraction def
                                 (string->number (string-append      ; => 15/16
                                   (car fracs) "/"
                                   (if (null? (cdr fracs)) "1" (cadr fracs))))))
                               (scale-dur "\\scaleDurations "))
                          (set! after-grace (cons ; before and after the grace section
                            (string-append scale-dur (number->string frac) ; "\\scaleDurations 15/16 s4\\f {"
                                                     " s" dur dyn
                                           " { ")
                            (string-append " } "                           ; "} \\scaleDurations 1/16 s4"
                                           scale-dur (number->string (- 1 frac))
                                                     " s" dur))))
                        (loop grace (1+ k)))
                ((string-null? grace) (loop (string-append "\\grace { " skip " }")
                                            (1+ k))) ; ; pos=7 then 13
                (else (loop (string-append
                              (substring grace 0 (1- (string-length grace)))
                              skip " }")
                            (1+ k)))))
            (if (> pos 0)   ; if at least one : char has been found
              (string-append
                (if after-grace (car after-grace) "")
                grace " <>" (if (< pos len) (substring str pos len) "")
                (if after-grace (cdr after-grace) ""))
              (string-append "<>" str)))))))
(reverse                   ;;;;;;;;;;;;;;;;;;;;;
  (fold
    (lambda (str prev-res)
     (let* ((len (string-length str))
            (last-par-i (string-rindex str #\)))    ;; last close parenthesis ) index (#f is not found)
            (pos-end (or (and last-par-i (1+ last-par-i)) ;; include last )
                         (string-index str #\space) ;; first space
                         len))
            (pos-start (or (and last-par-i (string-index str #\()) ;; 1st open parenthesis (skipping ' char if any)
                           0))
            (pos (let ((s (substring str pos-start pos-end))) ; a string representing a position
               (cond ((not last-par-i)          ; if no parenthesis ( ... )
                        (str->pos->pos-str s))  ; allows a user defined variable: ex A = #57
                      (else   ;; it will allow (A 2 8) => '(57 2 8) or ((+ A 29) 2 8) => '(86 2 8)
                        (let ((s-splitted (string-split (substring s 1) #\space)))
                          (if (let ((1st-word (car s-splitted)))                ; 1st word in s
                                (and (not (char=? (string-ref 1st-word 0) #\()) ; first char = "(" => it a sub list or a func
                                     (procedure? (eval-string 1st-word))))      ; is it a func ? (must return a pos !)
                            (str->pos->pos-str s) ; apply func and convert resulting pos to string. ex (cons A '(2 8.))
                            (str->pos->pos-str (string-append "(list " (substring s 1)))))))))) ; force evaluation
        ;(format #t "\n~s\n\tpos = ~a" str pos)   ; ← uncomment for debugging
        (if (= pos-end len)                           ; ← if no dyn specified :
          (let ((mom (pos-str->mom pos)))             ; that means that user want
            (let loop ((source prev-res)              ; to delete a previous pos-str
                       (dest '()))                    ; (same pos but dyn not empty)
                    (cond ((null? source)(reverse dest))
                          ((equal? mom (pos-str->mom (caar source))) ; if the same moment is found
                             (append (reverse dest)(cdr source)))    ; skip this element
                          (else (loop (cdr source) (cons (car source) dest)))))) ; otherwise, keep it
                                  ;; ↓ if dyn not empty, (<> pos-end len) :
          (let* ((dyn-section (string-trim (substring str pos-end len)))      ; skip pos
                 (dyn-with-tweaks (string-trim (sharp->tweaks dyn-section)))  ; # => tweaks
                 (music-str (colon->music-str dyn-with-tweaks)))              ; : => grace section
            ;(format #t "\ndyn-section=~a\ndyn-with-tweaks=~a\nmusic-str=~a\n"
            ;                dyn-section dyn-with-tweaks music-str)
            (acons pos music-str prev-res)))))
    '()
    (split-by-slash pos-dyn-str)) ; split by /
  ))

% (add-dynamics 'sym "5 mf / 9 _p cresc / (15 4) !"  will give
% (rm-with 'sym 5 #{ <>\mf #} / 9 #{ <>_\p\cresc #} / '(15 4) #{ <>\! #}
% 2 characters have a special action
% - the # character
%   mf#1#2.5#-3 results to:
%    <>-\tweak self-alignment-X #1 -\tweak extra-offset #'(2.5 . -3) \mf
%   mf#1  => self-alignment-X #1         | mf##2.5#-3 => extra-offset #'(2.5 . -3)
%   mf##2.5# => extra-offset #'(2.5 . 0) | mf##-3 => extra-offset #'(0 . -3)
% - the : character, followed by a duration expression
%  "3 <:16 :16*2 f" => \grace { s16\< s16*2 } <>\f  (measure 3)
%  to put after a dyn or a space (no dyn), and before a tweak section # with no spaces
%   a :: seq results to an afterGrace. "p::4 :8 f:16" => \afterGrace s4\p { s8 s16\f }
#(define (add-dynamics obj pos-dyn-str)
"Parses pos-dyn-str and results as :
(rm-with obj pos1 #{ <>\\dynamics1  #} / pos2 #{ <>\\dynamics2 #} ...)
obj is a symbol or a list of symbols.
The string `pos-dyn-str is a sequence of pos and dynamics, separted by slash /
The ' for list can be omitted : (11 4 8) instead of '(11 4 8).
All antislashes before dynamics are to be removed, but direction symbols - ^ _ are 
allowed. Several dynamics must be separated with spaces.
A pos with no dynamics tells the function to find and delete a previous dynamic occuring
in the same moment."
   (define (ret-if-failed)
     (let ((res (obj->music-list obj)))
       (if (null? (cdr res)) (car res) res)))
(if (string-null? pos-dyn-str)
  (ret-if-failed)
  (let ((res-str (fold ; returns arguments for rm-with as a string
          (lambda (pos-dyn prev-str)
            (string-append prev-str " " (car pos-dyn)              ; pos param
                                    " #{ " (cdr pos-dyn) " #}"))   ; music param
          ""
          (str->pos-dyn-list pos-dyn-str)))) ; make a list of pairs : see func above
    ; (format #t "****** position and music args:\n~a\n" res-str)
    (if (string-null? res-str) ; res-str can be empty even if pos-dyn-str is not
      (ret-if-failed)          ; for ex "3 f / 3" => ""
      (apply rm-with obj (eval-string (string-append "(list " res-str ")")))))))

%%%%%% assoc-pos-dyn, extract-pos-dyn-str, instru-pos-dyn->music, add-dyn %%%%%%%%
% user can here associate each pos-dyn to a set of instruments
% ex   vls = #'(vlI vlII) cors = #'(corI .. corIV) all = #'(fl htb cl ..)
%      assocDynList = #(assoc-pos-dyn
%        "1 p" 'corI / "5 mf" vls / "25 f / (31 4) <" cors / "33 ff" all ...)
% User can then extract the full pos-dyn-str for a specific instrument
% (see extract-pos-dyn-str), or make directly a skip music with all
% dynamics associated with this instrument.
%   (instru-pos-dyn->music 'vlI assocDynList)
%   => { s1*4 <>\mf s1*29 <>\ff s*...}
% User can also mix several instruments dynamics by using the 3 following boolean
% operators : or and xor
%   (instru-pos-dyn->music '(xor corI corII)  assocDynList)
%   => { <>\p s1*... }
%  The rules can be more complex '(or corII (xor corIII corIV))

#(define (assoc-pos-dyn . str-obj)
"syntax : (assoc-pos-dyn pos-dyn-str[s]A objA / pos-dyn-str[s]B objB ...)
Returns a list of pairs (pos-dyn-str . obj).
Each pos-dyn-str is a string as defined in add-dynamics function.
If a list of pos-dyn string is provided instead, assoc-pos-dyn converts
pos-dyn-strs in a compatible string \"pos-dyn-str1 / pos-dyn-str2 / ...\".
obj is an instrument or a list of instruments."
   (define (str . args) ; arguments are a pos-dyn string or a list of pos-dyn strings
     "Returns an pos-dyn string from args."
     (reduce-right (lambda(x prev) (format #f "~a / ~a" x prev))
                    ""
                    (flat-lst args)))
(let loop ((l (filter not-procedure? str-obj)) ;; skip /
           (res '()))
  (if (or (null? l)(null? (cdr l)))
    (reverse res)
    (loop (cddr l)                           ;; next str obj
          (acons (str (car l))                    ;; str
                 (flat-lst (obj->instru-list (cadr l))) ;; obj (a list of instruments)
                 res)))))				     %% res = '(str . obj) + res

                %%%%%%%%%%%%%%%%%%%%%%%%
                %%% extract-dyn-str  %%%

% xor is not defined in standard guile 1.8. We don't use this function directly
% but something called 'xor has to be defined
 #(if (not (defined? 'xor)) (ly:parser-define! 'xor (lambda(a b) (and a (not b)))))

#(define (extract-pos-dyn-str extract-code assoc-dyn-list)
  (define (clean-code elts)  ;; make a list with only symbols and sub-lists :
    (fold-right              ;; a sub-list will have at least 1 operator and 1 elt
      (lambda(x prev-lst)
        (cond
          ((instrument? x)(cons x prev-lst))  ;; instrument? keeps also 'and 'or 'xor
          ((pair? x)
             (let ((elt (car x)))
               (if (memq elt '(or and xor))   ;; elt is an operator
                 (let ((next (clean-code (cdr x))))
                   (if (pair? next)           ;; an operator needs operands
                     (cons (cons elt next) prev-lst) ;; the list become a sub-list
                     prev-lst))
                 (append (clean-code x) prev-lst))))    ;; the list is merged
          (else prev-lst)))
      '()
      (if (symbol? elts) (list elts) elts))) ;; extract-code is a symbol or a pair
                 ;;;;;;;;;;
   (define (in? elts l)
     (let ((operator (car elts))) ;; elts not empty
       (cond
         ((eq? operator 'and)    ;; elts has at least 2 elts (see clean-code)
            (let loop ((elt (cadr elts)) (next (cddr elts)))
              (and (if (symbol? elt)
                     (memq elt l)
                     (in? elt l)) ;; elt is a sub-list
                   (or (null? next)
                       (loop (car next) (cdr next))))))
         ((eq? operator 'or)
            (let loop ((elt (cadr elts)) (next (cddr elts)))
              (or (if (symbol? elt)
                    (memq elt l)
                    (in? elt l))
                  (and (pair? next)
                       (loop (car next) (cdr next))))))
         ((eq? operator 'xor)
            (let loop ((elt (cadr elts)) (next (cddr elts)))
              (and (if (symbol? elt)
                     (memq elt l)
                     (in? elt l))
                   (or (null? next)
                       (not (loop (car next) (cdr next)))))))
         (else (memq operator l)))))  ;; only if extract-code was a symbol
                 ;;;;;;;;;;
(let ((extract-code (clean-code extract-code))) ;; 'or 'and 'xor will not be filtered
  (if (null? extract-code)
    ""
    (fold
     (lambda(x prev)  ;; x = (pos-dyn-str . list-of-all-instru-with-it)
       (if (in? extract-code (cdr x))
         (let ((str (car x)))
           (if (string-null? prev)
             str
             (string-append prev " / " str)))
         prev))
     ""
     assoc-dyn-list))))

#(define (instru-pos-dyn->music extract-code assoc-dyn-list)
(add-dynamics
   (skip-of-length global) ;
   (extract-pos-dyn-str extract-code assoc-dyn-list)))

%% Be carefull : this macro will work, only if you have defined 'assocDynList. For ex :
%%   assocDynList = #(assoc-pos-dyn "1 p" 'corI / "1 f" 'vibra / "5 pp" cls / ...)
#(define-macro (add-dyn extract-code)
   `(instru-pos-dyn->music ,extract-code assocDynList))

%% the 2 following functions can be usefull if you want to split a only-dyn-music
%% into 2 \dynamics staff above and below of a central main staff
#(define (split-dynamics music symUp symDown defdir)
"Associate in the parser the 2 symbols symUp and symDown to music, but
dynamics with direction defdir goes to symUp, other to symDown."
   (define (myfilter m dir)
     (music-filter
       (lambda(x)
         (not (and (memq (name-of x) dyn-list) ; dyn-list is defined in chordsAndVoices.ly
                   (= dir (ly:music-property x 'direction defdir)))))
       (ly:music-deep-copy m)))
(let ((dynUp (myfilter music -1))
      (dynDown (myfilter music 1))
      (ret #f))
  (cond
    ((symbol? symUp) (def! symUp dynUp))
    ((equal? symUp #t) (set! ret dynUp)))
  (cond
    ((symbol? symDown) (def! symDown dynDown))
    ((equal? symDown #t) (set! ret (if ret (list ret dynDown)
                                           dynDown))))
ret))

dynSetDir = #(define-music-function (music direction)(ly:music? number?)
(let ((dirs (delete direction (list 1 -1 0))))
  (music-map
    (lambda(m)
      (let((dir (ly:music-property m 'direction #f)))
        (if (and dir (member dir dirs))
           (ly:music-set-property! m 'direction direction))
        m))
    music)))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% pitches %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define (display-transpose music amount)
"Print notes `amount steps higher or lower"
#{
  <<
    $music
    {
      \set staffLineLayoutFunction = #(lambda (x)(+ (ly:pitch-steps x) amount))
      $(moment->skip                                 ; set length a very little
            (ly:moment-sub (ly:music-length music)   ; shorter than `music.
                           (ly:make-moment 1/65536)))
      \unset staffLineLayoutFunction
    }
  >>
#})

#(define (fix-pitch music arg1 . other-args)
"Fixes pitches of music notes, to pitch arg1 or to (ly:make-pitch -1 arg1 0) or
to (ly:make-pitch arg1 arg2) or to (ly:make-pitch arg1 arg2 arg3)"
(let ((fix-pitch (case (length other-args)
                   ((2 1)(apply ly:make-pitch (cons arg1 other-args)))
                   ((0) (if (ly:pitch? arg1) arg1 (ly:make-pitch -1 arg1 0)))
                   (else (ly:error "fix-pitch syntax error : too many arguments")))))
   (music-map
     (lambda(evt) ; noteEvent? is defined in chordsAndVoices.ly
       (if (noteEvent? evt)(ly:music-set-property! evt 'pitch fix-pitch))
       evt)
     (note 1 music)))) % copies music and keeps only the first note in chords

#(define ((set-fix-pitch . args) music)
(apply fix-pitch music args))

#(define ((set-range range) music)   ; see checkPitch.ly
(correct-out-of-range music range))  % ex :  range = { c, c'' } or <c c'>

% syntax : (pitches->percu music 'snare / #{ f #} 'bassdrum / #{ e #} 'lowtom)
#(define (pitches->percu music percu-sym-def . args)
"Converts all notes to a drum type note.
Optionnal arguments are a suite of one pitch immediately following by a percu-sym (a 
symbol name defined for a drumStyleTable of a drumStaff). 
If a note in music has the same pitch of one of these pitches, his drum-type property
will be assigned to the corresponding percu-sym.
If no pitch is matching, percu-sym-def is the default percu-sym.
A slash / can optionally be used to separate all pitch percu-sym sections"
   (define (lst->hash-table lst)
     (let* ((len (length lst))
            (ht (make-hash-table (quotient len 2)))) ; groups elts by 2
       (if (< len 2)
         ht
         (let loop ((lst lst) (next-len (- len 2)))
           (hash-set! ht (let ((p (first lst))) ; a pitch or a number, as in fix-pitch
                            (if (ly:pitch? p) p (ly:make-pitch -1 p 0)))
                         (second lst))
           (if (< next-len 2)
             ht
             (loop (cddr lst) (- next-len 2)))))))
(let* ((ht (lst->hash-table (filter not-procedure? args))) ; remove possibly trailing /
       (func (lambda(note) (ly:music-set-property! note 'drum-type
                (hash-ref ht (ly:music-property note 'pitch #f) percu-sym-def)))))
  (for-each func (extract-named-music music 'NoteEvent))
  music))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% fragments and anchors %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% fragments %%%%%%%%%%%%%%%
% To use like this :
%  myfrag = {\anchor #'A musicA
%            \anchor #'B musicB
%            \anchor #'C musicC etc...}
%  #(let((frag (set-frag myfrag)))
%    (rm 'corII 20 (frag 'B)) ; remplace by musicB, music found in measure 20
%     ... etc)

% A variant of \musicAt
#(define ((set-frag music) sym)
"If an anchor associated with `sym is found in `music, the function extracts the music
beetween it and the following anchor. If several anchors match with `sym, the
function returns a sequential-music of all the musics extracted."
(let* ((res (fold-right ;; iteration from end of anchors list
         (lambda(entry prev-list) ; anchor entry = '(moment . sym) or '(moment . syms)
           (let ((mom1 (car entry))
                 (mom2 (car prev-list))
                 (syms (cdr entry)))
             ;(display anchor)(display "\n")
             (cons mom1        ; will be mom2 in the next iteration (= previous anchor)
               (if (and syms   ; #f only in 1st iteration (last anchor = (list length(fragments) #f)
                        (memq sym syms)
                        (ly:moment<? mom1 mom2))
                 (cons (extract music mom1 mom2)(cdr prev-list)) ; extract music
                 (cdr prev-list)))))  ; skip mom2
         (list ZERO-MOMENT)           ; dummy value (prev-list must not be empty)
         (cons (list ZERO-MOMENT sym) ; at beginning, no anchor will mean all symbols match
           (anchor->list music #f)))) ; get all anchors from beginning
       (sqm (map delete-anchors  ; cleaning
                 (cdr res))))    ; skip mom1
  (if (= (length sqm) 1)
    (car sqm)
    (make-sequential-music sqm))))
%{
#(define ((set-frag music) . anchors)
"If an anchor (a symbol) is found in music, the function extracts the music
beetween it and the following anchor. If several anchors match with `sym, the
function returns a sequential-music of all the musics extracted."
   (
(let* ((res (fold-right ;; iteration from end of anchors list
         (lambda(entry prev-list) ; anchor entry = '(moment . sym) or '(moment . syms)
           (let ((mom1 (car entry))
                 (mom2 (car prev-list))
                 (syms (cdr entry)))
             ;(display anchor)(display "\n")
             (cons mom1        ; will be mom2 in the next iteration (= previous anchor)
               (if (and syms   ; #f only in 1st iteration (last anchor = (list length(fragments) #f)
                        (memq sym syms)
                        (ly:moment<? mom1 mom2))
                 (cons (extract music mom1 mom2)(cdr prev-list)) ; extract music
                 (cdr prev-list)))))  ; skip mom2
         (list ZERO-MOMENT)           ; dummy value (prev-list must not be empty)
         (cons (list ZERO-MOMENT sym) ; at beginning, no anchor will mean all symbols match
           (anchor->list music #f)))) ; get all anchors from beginning
       (sqm (map delete-anchors  ; cleaning
                 (cdr res))))    ; skip mom1
  (if (= (length sqm) 1)
    (car sqm)
    (make-sequential-music sqm))))
%}

#(define (music->list music)
"Extract the list of the first sequential or simultaneous music encountered
in `music."
(filter
  (lambda(evt)
    (and (defined-music? evt)
         (not (eq? 'VoiceSeparator (name-of evt)))))
  (let loop ((m music))
    (let ((e (ly:music-property m 'element))
          (es (ly:music-property m 'elements)))
      (cond ((ly:music? e) (loop e));; if music begins with \relative for example
            ((memq (name-of m) '(SequentialMusic SimultaneousMusic))
                es)
            (else (list music)))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% tempo markings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For lilypond < 2.22, \note-by-dur-str do the same as the markup function \note .
% In lilypond 2.22, the duration-str argument of \note is now a duration (ly:duration?)
#(define-markup-command (note-by-dur-str layout props duration-str dir)(string? number?)
"Like \\note with a duration argument as a string"
   (define (str->log str)
     (let ((i (list-index string=? '("breve" "longa" "maxima") (circular-list str))))
       (if i (- (1+ i)) ; index 0 ("breve") -> -1 , index 1 -> -2 etc
             (let ((n (string->number str)))
               (if (and (integer? n) (> n 0))
                 (ly:intlog2 n)
                 (ly:error (_ "not a valid duration string: ~a") duration-str))))))
  (let ((split-list (string-split duration-str #\.))) ;; "4.."   -> (list "4" "" "")
     (note-by-number-markup layout props
        (str->log (car split-list)) (length (cdr split-list)) dir)))

#(define-markup-command (note=note layout props note1 note2 open-par close-par)
                                               (string? string? markup? markup?)
(let((note1-without-stem #{ \markup \note-by-dur-str #note1 #0 #}) ; avoid center alignment pbs
     (note2-without-stem #{ \markup \note-by-dur-str #note2 #0 #})) ; #0 => no stem
  (interpret-markup layout props
    #{ \markup \concat \smaller \raise #-0.1 \general-align #Y #DOWN {
         #open-par
         \smaller \general-align #Y #DOWN
           \with-dimensions-from #note1-without-stem \note-by-dur-str #note1 #1
         \hspace #0.3 "=" \hspace #0.1
         \smaller \general-align #Y #DOWN
           \with-dimensions-from #note2-without-stem \note-by-dur-str #note2 #1
         #close-par
     } #})))

#(define-markup-command (note=tempo layout props note tempo open-par close-par)
                                                 (string? string? markup? markup?)
(let((note-without-stem #{ \markup \note-by-dur-str #note #0 #}))
  (interpret-markup layout props
    #{ \markup \smaller \raise #-0.1 \general-align #Y #DOWN \concat  {
         #open-par
         \smaller \general-align #Y #DOWN
           \with-dimensions-from #note-without-stem \note-by-dur-str #note #1
         \hspace #0.3 "=" \hspace #0.1
         #tempo
         #close-par
    } #})))

#(define* (metronome mvt note arg #:optional (txt "") open-par close-par)
"Return a markup like \\tempo does. mvt and note are string.
 If arg is a number, arg represents the number of ticks per minute, else it is like the note
 parameter : a string reprensenting a duraation of a note : \"4.\" for ex.
 txt is \" env.\" or \" ca.\" appenned after the number of ticks per minute. 
 open-par and close-par are the characters around the \"note = arg\" markup (parenthesis by default)"
(cond
  ((and (equal? mvt "")(not open-par))
     (set! open-par "")
     (set! close-par ""))
  (else
     (if (not open-par) (set! open-par "("))
     (if (not close-par) (set! close-par ")"))))
(if (integer? arg)
  (markup #:line
             (mvt #:note=tempo note (string-append (number->string arg) txt) open-par close-par))
  (markup #:line
             (mvt #:note=note note arg open-par close-par))))

#(define (tempos where-pos mvt . args) ;
"Syntax 1 : tempos where-posA mvtA [spaceA] / where-posB mvtB [spaceB] / ...
Syntax 2 : tempos obj where-posA mvtA [spaceA] / where-posB mvtB [spaceB] / ...
Adds a command : \tempo mvt, at where-pos in 'global (syntax 1) or in obj (syntax 2).
If a space is specified, move horizontaly the tempo markup by space units."
(define (do-tempos obj args)
(let loop ((res '()) ; a list of list
           (elt '()) ; an elt from res
           (l (reverse args))) ; all arguments are reversed, so l will end by
                               ; a pos preceded by a markup
  (if (pair? l)
    (let ((arg (car l))
          (next (cdr l)))
      (if (markup? arg)                   ; is arg a mvt argument ?
        (loop (cons (cons (car next)      ; yes ? where-pos is just after. (reversed list)
                          (cons arg elt)) ; add where-pos and mvt to elt.
                    res)                  ; add elt to res
              '()                         ; reset elt
              (cdr next))                 ; skip the 2 elements added
        (loop res
              (cons arg elt)              ; just add arg to elt
              next)))
    (map (lambda(entry)                             ; entry = '(where-pos mvt [space])
           (rm obj (first entry)                    ; where-pos
             (if (< (length entry) 3)               ; no 3rd element (space argument) ?
               #{ \tempo \markup #(second entry) #} ; mvt.
               #{ \tempo \markup { \hspace #(third entry) #(second entry) } #})))
         res))))
(let ((args (cons where-pos (cons mvt (filter not-procedure? args))))) ; skip slashes /
  (if (pos? where-pos)
    (do-tempos 'global args) ; syntax 1
    ; replace 'global by the 1st args (syntax 2). (length args) must be >= 3
    (do-tempos (car args) (cdr args)))))

% (signatures 1 "3/4" 10 "3,2 5/8" 20 "4/4") =>
% (rm-with 'global 1 #{ \time 3/4 } / 10  #{ \time 3,2 5/8 #} / 20 #{ \time 4/4 #})
#(define (signatures pos sig-str . args)
"Inserts time signatures in 'global.
syntax: (signatures pos1 sig-str1 [/] pos2 sig-str2 [/] ...)
A sig-str is all the argument(s) of a \\time command inserted between 2 quotation marks.
A pos is the barnumber where to insert the time signature."
(let ((args (fold-right
        (lambda(arg prev)
          (if (string? arg)
            (cons (eval-string (string-append "#{ \\time " arg " #}")) prev)
            (cons arg prev)))
        '() (cons pos (cons sig-str (filter not-procedure? args))))))
  (apply rm-with 'global args)))

% (keys 1 "c major" 20 "c minor" 30 "g") =>
% (rm-with 'global 1 #{ \key c \major } / 10  #{ \key c \minor #} / 20 #{ \key g \major #})
#(define (keys pos key-mode-str . args)
"syntax 1: (keys posA key-mode-strA [posB key-mode-strB [...]])
syntax 2: (keys obj posA key-mode-strA [posB key-mode-strB [...]])
Inserts key changes in obj (syntax 2) or in 'global (syntax 1), at the specified position pos.
A key-mode-str is the same 2 arguments: key + mode of the \\key command, inserted between
2 quotation marks. The default mode is \\major if mode is omitted. The backslash \\ before
the mode can be omitted too: major instead of \\major. ."
(define (callback arg prev) ; callback for fold-right. arg is a string or a pos
  (if (string? arg)
    (cons (eval-string
            (let* ((key-modes (filter not-string-null? (string-split arg #\space)))
                   (modes (cdr key-modes)))
              (string-append
                "#{ \\key " (car key-modes) " "
                (if (null? modes)
                  "\\major"
                  (let ((mode (car modes))) (if (char=? (string-ref mode 0) #\\)
                    mode (string-append "\\" mode))))
                " #}")))
          prev)
    (cons arg prev))) ; arg is a pos
(let ((args (cons pos (cons key-mode-str (filter not-procedure? args)))))
   (if (pos? pos)
     (apply rm-with 'global (fold-right callback '() args)) ; syntax 1
     ; replace 'global by the 1st args (syntax 2). (length args) must be >= 3
     (apply rm-with (car args) (fold-right callback '() (cdr args))))))

#(define (marks pos . other-pos)
"syntax: (marks [obj] pos1 pos2 ...)
Inserts a \\mark \\default to each positions pos, in 'global or in obj if
specified."
(let* ((mark (make-music 'MarkEvent))         ; \mark \default
       (args (if (pos? pos)
         (cons 'global (cons mark (cons pos other-pos)))
         (cons pos (cons mark other-pos)))))  ; pos = obj
  (apply x-rm args)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% working with patterns %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% rhythm patterns : see changePitch.ly

#(define (cp . args) ; see change-pitch in changePitch.ly
"syntax : (cp [keep-last-rests?] pattern[s] music[s])
Applies change-pitch with pattern and music as arguments. 
Returns a music, or a list of musics if one of these arguments are a list of musics.
If pattern ends with rests, they are also added after the very last note, unless the
boolean keep-last-rests? is set to #f: in this case, the returned musics will end 
with a note."
(receive (booleans params) (partition boolean? args)
  (if (< (length params) 2) (ly:error "bad syntax : (cp [keep-last-rests?] pattern music) !"))
  (let ((pats (map expand-notes-and-chords (flat-lst (car params))))
        (musics (let ((l (map expand-notes-and-chords (obj->music (flat-lst (cdr params))))))
          (if (or (null? booleans)(car booleans)) ; keep-last-rests?
            (map (lambda(m) (seq m (moment->skip ZERO-MOMENT))) l)
            l)))
        (do-cp (lambda args (apply change-pitch (map ly:music-deep-copy args)))))
    (let ((delta (- (length musics) (length pats))))
      (cond
        ((> delta 0) (map do-cp (circular-list (last pats)) musics))
        ((< delta 0) (map do-cp pats (circular-list (last musics))))
        (else (let ((res (map do-cp pats musics)))
          (if (null? (cdr res)) (car res) res))))))))

%%% shortcuts
% by def, keep-last-rests? is #f for set-pat, #t for set-pitch
#(define ((set-pat . args1) . args2 ) (apply cp (append args1 (cons #f args2))))
#(define ((set-pitch  . args1) . args2) (apply cp (append args2 args1)))

#(define-macro (cp1 obj) `(cp patI ,obj))
#(define-macro (cp2 obj) `(cp patII ,obj))
% Syntax error in guile 2 with compose: ((compose cp1 (set-octave 2)) music) =>
%    "unknown location: source expression failed to match any pattern in form cp1"

#(define (cp-with obj pos new-notes . others-pos-new-notes)
(let loop ((pos pos)
           (new-notes new-notes)
           (args (filter not-procedure? others-pos-new-notes))) ;; del slashes /
  (let ((res (apply-to obj (set-pitch new-notes) pos 'end)))
    (if (< (length args) 2)
      res
      (loop (first args)
            (let((arg2 (second args)))
              (if (promise? arg2) (force arg2) arg2))
            (list-tail args 2))))))

%% (tweak-notes-seq `(1 2 3 2 1 (3 . ,(set-transp -1 0 0)))
%%                  #{ d f a c e g #})
%% => { d f a f d a, c e g e c g, }
#(define (tweak-notes-seq n-list mus)
"n-list is a list of numbers, each representing the nth note of mus.
tweak-notes-seq makes repeatedly a sequence of the corresponding notes.
When the last numbers in n-list is reached, the process restart to the beginning but
numbers are shifted by a amount equal to the greatest number of n-list. The process continues
until a number has no more corresponding notes in mus.
Instead of a simple number n, it is possible to specify a pair (n . music-function).
music-function is then applied to the nth note"
(let* ((elts (extract-named-music mus '(EventChord NoteEvent RestEvent SkipEvent)))
       (number+? (lambda(n)(and (number? n)(> n 0))))
       (n-list (filter-map (lambda(x)(or (and (number+? x) (cons x #f))
                                         (and (pair? x)(number+? (car x)) x)))
                           n-list))
       (max-n (fold-right (lambda(x prev)(max prev (car x))) 1 n-list)))
  (if (null? n-list)(set! n-list (list (cons 1 #f))))
  (let loop1 ((l1 elts)
              (len (length elts))  ; we don't want to compute list length each loop
              (res1 '()))
    (if (null? l1)
      (make-sequential-music (reverse res1))
      (let loop2 ((l2 n-list)
                  (res2 res1))
         (if (null? l2)
           (loop1 (list-tail l1 max-n)(- len max-n) res2)
           (let((n (car (car l2)))
                (func (cdr (car l2))))
             (if (<= n len)
               (loop2 (cdr l2)
                      (let((m (ly:music-deep-copy (list-ref l1 (1- n)))))
                        (cons (if func (func m) m) res2)))
               (make-sequential-music (reverse res2))))))))))

#(define ((set-tweak-notes-seq n-list) mus)
  (tweak-notes-seq n-list mus))

%%% articulation patterns
% #(define ((set-arti pattern) obj)  ; see copyArticulations.ly
%    (define (func mus) (copy-articulations pattern mus))
% ((music-func->obj-func func) obj))
%
% #(define (ca pattern obj)
% ((set-arti pattern) obj))

#(define (ca . args)  ; see copyArticulations.ly
"syntax : (ca pattern[s] music[s])
Copies articulations from pattern to music, and returns music.
If at least one argument is a list (a list of musics or a list of instruments),
the function returns a list of musics."
    (define (func p m) (ly:music-deep-copy (copy-articulations p m)))
(if (null? (filter pair? args)) ; if no list (only music)
  (apply func (map obj->music args))
  (apply map func
             (map (lambda(x)(if (pair? x)(map obj->music x)(circular-list (obj->music x))))
                  args))))
#(define ((set-arti pattern) obj)
(ca pattern obj))

#(define ((set-del-events event-sym . args) obj)
"Delete all events named `event-sym. Several events can be specified,
or even a list of event."
(let ((events-list (flat-lst event-sym args)))
  (define (func mus)
     (music-filter
       (lambda (evt)
         (not (memq (ly:music-property evt 'name) events-list)))
       mus))
  ((music-func->obj-func func) obj)))

%%% music patterns
#(define (fill-with pat from-pos to-pos)
"Repeats the pattern `pat, as much times needed to fill the range [from-pos 
to-pos], cutting if necessary the last repeat to fit exactly the range."
(if (cheap-list? pat)
  (map (lambda(m)(fill-with m from-pos to-pos)) pat)
  (let* ((len (ly:music-length pat))
         (delta (pos-sub to-pos from-pos))
         (n (mom->integer (mom-div delta len)))  ;; see extractMusic.ly for the mom-xxx func
         (remain (mom-sub delta (mom-imul len n))))
         ;(format #t "n = ~a\n remain = ~a\n" n remain)
    (cond ((equal? remain ZERO-MOMENT) (n-copy n pat)) ; see changePitch.ly
          ((= n 0) (extract pat ZERO-MOMENT remain))
          (else (seq (n-copy n pat)(extract pat ZERO-MOMENT remain)))))))

#(define (fill-generic obj pat-or-pats from-pos to-pos func . args)
"Internal function used by fill and fill-percent"
(let loop ((from-pos from-pos)
           (to-pos to-pos)
           (args (filter not-procedure? args)))  ;; del all /
  (let* ((ret (rm obj from-pos (if (ly:music? pat-or-pats)
                    (func pat-or-pats from-pos to-pos) ; if  music
                    (map func pat-or-pats              ; if list
                         (circular-list from-pos)
                         (circular-list to-pos))))))
    (if (null? args)
      ret
      (let ((x (car args)))
        (if (promise? x)(set! x (force x)))
        (if (or (ly:music? x)(ly:music-list? x))
          (begin (set! args (cdr args))
                 (set! pat-or-pats x)))
        (if (>= (length args) 2)
          (loop (first args)(second args)(list-tail args 2))
          ret))))))

#(define (fill obj pat-or-pats from-pos to-pos . args)
"Replace at from-pos, each obj by (fill-with pat from-pos to-pos).
You can specify several sections to fill by the following way :
 (fill obj patA posA1 posA2 / patB posB1 posB2 / etc...)
If patX is omitted, the prev pat is assumed."
(apply fill-generic obj pat-or-pats from-pos to-pos
        (lambda(pat pos1 pos2)(fill-with pat pos1 pos2))
        args))

#(define (fill-percent obj pat-or-pats from-pos to-pos . args)
"idem than fill but produce percent repeat"
(apply fill-generic obj pat-or-pats from-pos to-pos
        (lambda(pat pos1 pos2)
          (let ((delta (pos-sub pos2 pos1)))
            (make-music 'PercentRepeatedMusic ; see extractMusic.ly for mom->integer and mom-div
             'repeat-count (mom->integer (mom-div delta (ly:music-length pat)))
             'element pat)))
        args))

#(define ((set-ncopy n) music)
   (n-copy n music))

%%%%%%%%%%%%%%%%%%%%%%%% Working with ossia %%%%%%%%%%%%%%%%%%%%%%
addStaffSet = {
  %%s1*0^"Ossia"
  \once \omit Staff.KeySignature
  \once \omit Staff.Clef
  \once \omit Staff.TimeSignature
  \set Staff.explicitKeySignatureVisibility = #'#(#f #f #t)
  \magnifyStaff #1/2
  % \override Staff.VerticalAxisGroup.staff-staff-spacing =
  %           #'((basic-distance . 2)
  %              (minimum-distance . 2)
  %              (padding . 2)
  %              (stretchability . 1))
}

#(define* (add-staff obj mes-n musics
            #:optional settings ; with #:optional (settings addStaffSet) errors :
                                ; "Erreur de segmentation  (core dumped)lilypond"
            #:key (alignAboveContext ""))
(if (and settings (ly:music? settings))
  (set! settings (seq addStaffSet settings))
  (set! settings addStaffSet))
(for-each
  (lambda(instru music alignAboveCtx)
    (let ((op (if (string? alignAboveCtx)
                (list (list 'assign 'alignAboveContext
                              (if (string-null? alignAboveCtx)
                                (symbol->string instru)
                                alignAboveCtx)))
                (list))))
      (rm instru mes-n (sim (make-music 'ContextSpeccedMusic 'create-new #t
                                        'property-operations op
                                        'context-type 'Staff 'element (seq settings music))
                            (em instru mes-n `(,mes-n ,(ly:music-length music)))))))
  (obj->instru-list obj)
  (obj->music-list musics)
  (if (cheap-list? alignAboveContext)
    alignAboveContext
    (circular-list alignAboveContext))))

%%%%%%%%%%%%%%%%%%%%%%%%% compile only a section of score %%%%%%%%%%%%%%%%%%%%%%
#(define (show-score from-pos to-pos)
"Lilypond will show your score only between range [from-pos to-pos]"
(let* ((skip-type-set (lambda (skip?)
          (make-music 'ContextSpeccedMusic 'context-type 'Score 'element
            (make-music 'PropertySet 'value skip? 'symbol 'skipTypesetting))))
       (from (pos->moment from-pos))
       (during (ly:moment-sub (pos->moment to-pos) from))
       (end (ly:moment-sub (ly:music-length global) during))
       (skips-seq (seq (skip-type-set #t)
                       (moment->skip from)
                       (skip-type-set #f)
                       (moment->skip during)
                       (skip-type-set #t)
                       (moment->skip end))))
 (ly:parser-define! 'global (sim global skips-seq))))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% miscellaneous %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A..Z AA..AZ BA..BZ ZA..ZZ AAA...AAZ ...but skip letter I
#(define (index->string-letters i) ;; i 0-based index ;
"Write i in base 25, and assign each digit to one (or more) capital letter(s), skipping I"
    (define deltaA  (char->integer #\A)) ;; code point of the first capital letter
    (define deltaI (char->integer #\I))  ;; code point of I
(let* ((delta (+ (remainder i 25) deltaA)) ; the code point in A-Z
       (s (string (integer->char (if (< delta deltaI) delta (1+ delta))))))
            ;; get the letter skipping I letter
  (if (< i 25)
    s          ; one letter
    (string-append
      (index->string-letters (1- (quotient i 25))) ; 1- because the index is 0-based
      s))))

% a function to associate a letter to measure numbers
% #(define* (def-letters measure-list #:optional (show-infos #t)
%                                                (index->string index->string-letters)
%                                                (start-index 0))
#(define (def-letters measure-list . args)
"Associate in the parser each measure number of the given list to a symbol.
By default, each symbol name is a capital letter A-Z less letter I (26 - 1  =
25 possibilities). If measure-list count exeeds 25, a second, a third... capital 
letter is added to the left"
   (define (pred->arg pred? default)
     (let ((filtered-args (filter pred? args)))
       (or (and (pair? filtered-args)
                (car filtered-args))
           default)))
(let ((index->string (pred->arg procedure? index->string-letters))
      (start-index (pred->arg index? 0))
      (show-infos (pred->arg boolean? #t)))
  ; (for-each (lambda(x) (format #t "~a " (index->string x))) (iota 652)) ; uncomment to test
  (for-each
    (lambda (x y) ;; associate symbol 'A 'B 'C... to an elt of the list
      (let* ((s (index->string x))
             (sym (string->symbol s)))
        (if (defined? sym)
          (let((new-s (string-append "_" s)))
            (if show-infos (ly:message
"def-letters infos :\n  symbol ~a already defined. New symbol will be : ~a" s new-s))
            (set! sym (string->symbol new-s))))
        (ly:parser-define! sym y)))
    (iota (length measure-list) start-index)
    measure-list)
  ;; return the original list))
  measure-list))

%%%%%%%%%%%%%%%%%%%%%%%%%% clean export of instruments %%%%%%%%%%%%%%%%%%%%%%%%%
%% export-instruments function uses display-lily-music to export each instrument in a file.
%% As display-lily-music adds a line break only when a bar-check is found, the add-bar-cheks
%% function below, was designed to basically add a bar-chek "|" after each measure.
%% However, some events that spread over several measures (like multi-measure rests) should not be
%% splitted into several one. For a simultaneous music, only inner voices should be splitted.
#(define* (add-bar-checks music #:optional (music-start-pos (moment->pos ZERO-MOMENT)))
;; utilities
   (define whole-event-names ; events that must not be splitted
     '(NoteEvent EventChord RestEvent MultiMeasureRestMusic SkipEvent SkipMusic SimultaneousMusic))
   (define (make-whole-event-list music i)
      "Makes a list of all whole-event in music, in reverse order, creating if i is
       an integer, a property 'index for each event."
     (let ((set-index (if i (lambda(m)(set! i (1+ i))
                                      (ly:music-set-property! m 'index i)
                                      m) ; returns (not #f)
                            (lambda(m) m)))) ; identity
       (fold-some-music (lambda(m)(and (memq (name-of m) whole-event-names)
                                       (set-index m)))
                        cons '() music)))
   (define (event->index m) (ly:music-property m 'index #f))
   (define (index->event whole-event-list i)
     (and i (any (lambda(m) (let ((index (event->index m)))
                              (and index (= index i) m)))
                 whole-event-list)))
   (define (copy-dur from-whole-evt to-whole-evt) ; make to work also with chords
     (define (dur-evt? m) (ly:music-property m 'duration #f))
     (define (dur-evts-in evt) (fold-some-music dur-evt? cons '() evt))
     (for-each (lambda(from to)(ly:music-set-property! to 'duration
                                  (ly:music-property from 'duration)))
               (dur-evts-in from-whole-evt)
               (dur-evts-in to-whole-evt)))
;; main
   ; (display (ly:moment? (pos:remain music-start-pos)))
(let* ((music ((set-del-events 'BarCheck) music)) ;; del previous BarChecks to avoid double Barcheck
       (1st-i (pos:num music-start-pos)) ; bar number
       (partial (and (= 1st-i first-measure-number)
                     (extract music ZERO-MOMENT (pos->moment first-measure-number))))
       (original-whole-events (reverse (make-whole-event-list music 0))))
  (let loop ((i 1st-i) ; the measure number
             (res (if (and partial (defined-music? partial))    ; if partial...
                    (list (seq partial (make-music 'BarCheck))) ; ... add it.
                    '())))
    (let ((m (em music i (1+ i) music-start-pos)))  ;; extract 1 measure
      ;(display-scheme-music m)
      (if (defined-music? m) ;; true while end not reached
        (let* ((l (make-whole-event-list m #f)) ; event of the current measure
               (sim (any (lambda(m)(and (eq? (name-of m) 'SimultaneousMusic) m)) l)))
          (if sim
            (let* ((sim-start-mom (mom-sub (pos->moment (1+ i))(ly:music-length sim)))
                   (sim-start-pos (moment->pos sim-start-mom))
                   (index (event->index sim))
                   (original-sim (index->event original-whole-events index)))
              (ly:music-set-property! sim 'elements (map
                    (lambda(voice)(if (equal? (ly:music-length voice) ZERO-MOMENT)
                                    voice (add-bar-checks voice sim-start-pos)))
                    (ly:music-property original-sim 'elements)))
              ;(display-lily-music m)
              (let* ((sim-end-mom (mom-add sim-start-mom (ly:music-length sim)))
                     (sim-end-pos (moment->pos sim-end-mom))
                     (new-i (if (equal? (pos:remain sim-end-pos) ZERO-MOMENT)
                       (pos:num sim-end-pos)                       ; sim ends at a bar-start
                       (let ((bar-pos (1+ (pos:num sim-end-pos)))) ; sim ends at the middle of the measure
                         (set! m (seq m (em music sim-end-pos bar-pos music-start-pos))) ; adds the remaining music
                         bar-pos))))
                 (loop new-i (cons (seq m (make-music 'BarCheck)) res))))
            (let* ((last-evt (car l)) ; get last whole-event in m (= first in l) : has it been cut ?
                   (index (event->index last-evt))
                   (original (index->event original-whole-events index)))
              (if original (copy-dur original last-evt))
              (let* ((m-end-mom (mom-add (pos->moment i) (ly:music-length m)))
                     (m-end-pos (moment->pos m-end-mom))
                     (new-i (if (equal? (pos:remain m-end-pos) ZERO-MOMENT) ; m ends at a bar-start ?
                              (pos:num m-end-pos) (1+ (pos:num m-end-pos)))))
                (if (> new-i i) (set! m (em music i new-i music-start-pos))) ; takes several measures
                (loop new-i
                      (if (equal? (ly:music-length m) ZERO-MOMENT)
                        (cons m res)
                        (cons (seq m (make-music 'BarCheck))
                              res)))))))
        ;; not defined-music? => end of music reached
        (cond ((null? res) (make-music 'Music))
              ((null? (cdr res)) (car res))
              (else (seq (reverse res)))))))))


#(define* (export-instruments instruments filename #:optional overwrite?)
"Save in `filename, all instruments of the `instruments list in the traditional 
manner : instrument-name = { music ... }, in absolute-mode.
`filename is created in the current directory.
If `filename already exists, the instrument definitions will be appended at the 
end of all pre-existing texts, unless if `overwrite? is set to #t"
(let ((port (open-file filename (if (equal? overwrite? #t)
                    "w"     ; Open for output in "write mode" (from scratch)
                    "a")))) ; Open for output in "append mode"
  (display (format #f
              "                 %%%%%% instruments export : ~a %%%%%%\n\n"
              (strftime "%c" (localtime (current-time)))) ; date and time
           port)
  (for-each
    (lambda(sym)
      (let* ((name (symbol->string sym))
             (music (reduce-seq (add-bar-checks (obj->music sym)))))
         ;(display "\n********** ")(display name)(newline)(display str1) ; for debug
         (display (string-append name " = ") port)
         (display-lily-music music port)
         (newline port)
         (newline port)))
    (flat-lst instruments))
  (close-port port)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
