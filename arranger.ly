\version "2.20.0"
%%%%%%%%%%%%%%%%%%%%%% version Y/M/D = 2020/07/04 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% For Lilypond 2.20 or higher %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main goal of arranger.ly is to provide a set of functions to make arrangements
% (for ex : a symphonic piece for a concert band (wood-winds and percussions only))
% The basic possibility is to allow the user to be able to insert a fragment of
% music, to simultaneously a whole set of instruments, and to several musical
% positions in one shot.
% For that, "positions" have had to be re-defined, here. A new timing location system
% is based on measure number as main position indicators (Lilypond use instead moments).
% The user has just, beforehand, to define a \global variable, in which he has to
% store all timing signature changes, separated by skips of appropriate length.
% ex :    global = { s1*4 \time 5/8 s8*5*7 \time 3/4 s4*3*6 etc ...}
% Others timing events like \partial, cadenzaOn/Off... are supported.
% The user then, calls the init function (see later) with a set of instruments as parameter.
% The immediat result is that each instruments are automatically filled by appropriate
% multi-measure rests (same length than \global). Even a starting rest is added if a
% \partial is found in \global).
% last changes :
%   new : em-with-func, copy-to-with-func, copy-out-with-func, extend apply-to syntax
%   new function : fill-percent build with fill-generic a func to buid new fill function
%   fix export-instruments : do not split MultiMeasureRest and SimultaneousMusic
%   rename set-notes to set-pitch
%   cp : allows an optional argument keep-last-rests?
%            (cp [keep-last-rests?] pattern music)
%   str->pos-dyn-list : allows tweaking X alignment
%      "13 ^mf#-0.5" means : at measure 13, -\tweak DynamicText.self-alignment-X #-0.5 ^mf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\include "chordsAndVoices.ly"  %% original name "chord.ly". See :
                                 %% LSR = http://lsr.di.unimi.it/LSR/Item?u=1&id=761
                                 %% LSR = http://lsr.di.unimi.it/LSR/Item?u=1&id=545
\include "changePitch.ly"        %% LSR = http://lsr.di.unimi.it/LSR/Item?id=654
\include "copyArticulations.ly"  %% LSR = http://lsr.di.unimi.it/LSR/Item?id=769
\include "addAt.ly"              %% http://code.google.com/p/lilypond/issues/detail?id=824
% \include "extractMusic.ly"     %% LSR = http://lsr.di.unimi.it/LSR/Item?id=542
                                 %% extractMusic.ly is already included in addAt.ly
% \include "checkPitch.ly"      %% LSR = http://lsr.di.unimi.it/LSR/Item?id=773
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
 (extract-range (expand-q-chords (ly:music-deep-copy music)) from to)))

%% x-extract is an extended version : used by em-with-func and rm
#(define (x-extract music-or-musics . args)
(if (pair? music-or-musics)
  (map (lambda(x) (apply x-extract x args)) music-or-musics)
  (apply extract music-or-musics args)))

%%%% replace %%%%
%% scheme function corresponding to the \replaceMusic function in extractMusic.ly
#(define* (replace music where replacement-music #:optional (start ZERO-MOMENT))
(let* ((repla (ly:music-deep-copy replacement-music))
       (repla-len (ly:music-length replacement-music))
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
If n is an integer (positive or negative), returns <moment 1/n>
If n is a rationnal, returns <moment n>
Any moment remains unchanged"
(cond
    ((integer? n) (ly:make-moment 1 n 0 0)) ; zeros needed ! (if n <= 0)
      ; if n = 0, (ly:make-moment 1 0 0 0) => ZERO-MOMENT
    ((ly:moment? n) n) ;can be usefull with scaled musics
    ((rational? n) (ly:make-moment n))
    (else ZERO-MOMENT)))

%% The main code in pos->moment is provided by measure-number->moment, which is
%% defined in "extractMusic.ly". idem for mom-add which allows args count > 2
#(define (pos->moment pos)
"Convert a measure-based position into a moment."
(let* ((pos-list (if (pair? pos) pos (list pos)))
       (mom (let ((n (car pos-list))
                  (m first-measure-number)) ; if n < m, take m later
         (cond ((not (integer? n)) ; can be 'end or 'begin
                  (if (eq? 'end n)(ly:music-length global) ;
                                  ZERO-MOMENT)) ; 'begin
               (else (measure-number->moment (max n m) m))))));;
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
  ((symbol? x) (primitive-eval x)) ;; try (module-ref (current-module) x)
  ((pair? x) (map obj->music x))
  (else x)))

#(define (obj->music-list obj)
(if (list? obj)
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
  (if (list? obj) obj (list obj))))

#(define (music-obj? obj)
"Is obj a music, or a list with only music elements ?"
(or (ly:music? obj)
    (and (pair? obj) ;; ly:music-list? perhaps ?
         (music-obj? (car obj))
         (let ((next (cdr obj)))
           (or (null? next)
               (music-obj? next))))))

#(define (music-func->obj-func music-func)
   (define (func obj)
     (if (list? obj)
       (map func obj)
       (music-func
         (ly:music-deep-copy (obj->music obj)))))
   func)

%% To allow a user to add a slash / to separate args (see x-rm for ex)
#(define (not-procedure? x)(not (procedure? x)))

%%%%  Utilities for making lists. %%%%
%%%% lst %%%%
%% can be use also by user.  example :
%% fls = '(piccolo flI flII)
%% all = (lst fls 'oboe 'bassoon) => '(piccolo flI flII oboe bassoon)
#(define (lst . args)
"Make a list with each elements of args, or if a element is a list, with the 
elements themselves of this element-list."
(fold-right
  (lambda(x prev-lst) ; (please, don't change list? by pair?)
     (if (list? x)(append x prev-lst)(cons x prev-lst)))
  '()
  args))

%% like lst but goes deeper.
#(define (flat-lst . args)
(fold-right
  (lambda(x prev-lst) ; (please, don't change list? by pair?)
     (if (list? x)(append (apply flat-lst x) prev-lst)(cons x prev-lst)))
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

%% x-pos
%% Making a list of pos from a pattern. To use for ex with x-rm like this :
%% music = { e'2 f' | g' f' | e'1 }
%% (apply x-rm 'music #{ c'8 c' c' #} (x-pos 1 3 '((n 8)(n 2 8)))) ;; use apply
%% => music = { e'8 c' c' c' f' c' c' c' | g' c' c' c' f' c' c' c' | e'1 }
#(define* (x-pos from-measure to-measure #:optional pos-pat (step 1))
"pos-pat is a quoted list of by-measure-number positions with a letter, typically n,
instead of the measure number.
x-pos expand this pattern, replacing n (the letter) by from-measure and increasing
repeatedly this value by step, while lesser to to-measure.
By default, pos-pat is '(n), step is 1"
(let ((func (if (not pos-pat)
              (lambda(n prev) (cons n prev))
              (lambda(n prev)
                (fold-right (lambda (x prev2)(cons x prev2))
                            prev
                            (let loop ((x pos-pat))
                               (cond ((symbol? x) n)
                                     ((pair? x)(map loop x))
                                     (else x)))))))
      (d (- to-measure from-measure)))
(fold-right func '() (iota (quotient (+ d (remainder d step)) step) ; count
                           from-measure
                           step))))
%{ #(map (lambda (x) (display x)(newline)) (list    ;; tests
 (x-pos 10 12)               ;; => (10 11)
 (x-pos 10 12 '(n (n 4)))    ;; => (10 (10 4) 11 (11 4))
 (x-pos 10 12 '(n (n 4)) 2)  ;; => (10 (10 4))
 (x-pos 10 13 '(n (n 4)) 2)  ;; => (10 (10 4) 12 (12 4))
 (x-pos 10 14 '(n (n 4)) 2)  ;; => (10 (10 4) 12 (12 4))
 )  %}

%% Guile 2.0 propose a compose function, not 1.8 :-(
#(define ((compose func-n . func-i) obj)
"((compose func3 func2 func1) obj) will result to
(func3 (func2 (func1 obj)))"
(if (null? func-i)
  (func-n obj)
  (func-n ((apply compose (car func-i)(cdr func-i)) obj))))

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
and optionnaly set the number of first measure."
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
 (copy-to destination sourceA posA1 posA2 / sourceB posB1 posB2 / etc...)
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
(let ((obj (if (list? obj) (flat-lst obj) obj))) ; no list inside list - symbols only
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
"syntaxe : (x-apply-to obj func from1 to1 / from2 to2 / etc ...
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
   (lambda(music) (music-map (lambda(m) (func m)
                                        m)
                             (ly:music-deep-copy music))))

#(define (xchg-music obj1 obj2 from-pos to-pos . other-from-to-pos)
"Exchange the music of the range [from-pos to-pos[ between obj1 and obj2"
   (define (sub-list->sym obj) ; Take the first not-list element of sub-lists
     (if (list? obj) (map (lambda(x)(let loop ((x x))
                            (if (list? x) (loop (car x)) x)))
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

% function rel : shortcut for \relative. Well, it is now not a simple shortcut as we create
% first a generic function to extend syntax for functions in the form : (func n music)
% functions concerned : rel, octave and octave+

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
    (if (integer? n) (cons n args)
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
% a code like #{ \relative c' $music #} is not compatible with all language
% so we use (ly:make-music-relative! music pitch)

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

#(define (split music1 music2)
"Equivalent to << music1 \\ music2 >> "
(sim music1
     (make-music (quote VoiceSeparator))
     music2))

#(define (at pos mus)
   (if (list? mus)(map (lambda(x)(at pos x)) mus)
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

#(define (x-em obj pos . args)
(fold-right (lambda(pos1 pos2 prev) (cons (em obj pos1 pos2) prev))
  '() (cons pos args) args))


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
"Syntax 1 : (set-transp o n a/2)         (o, n, a as integers, positive or negative)
Syntax 2 : (set-transp func(p))          (p as the current source pitch)
Apply ly:pitch-transpose to each pitch of `obj :
by delta pitch o (octaves), n (notename), a/2 (alterations) (syntax 1), or
by delta pitch returned by the callback function func (syntax 2).
`obj can be a symbol, a list of symbols, a music or a list of musics.
If `obj is a list, or if other arguments are given in obj-arg, the function
returns a flat list of transposed music."
(define delta #f)
(case (length args)
  ((1)(let ((func (car args)))
         (if (procedure? func) (set! delta func))))
  ((3)(if (every (lambda(r)(or (integer? r)(rational? r))) args) ;(number-list? args)
        (set! delta (lambda(dummy)(apply ly:make-pitch args))))))
(if (not delta) (ly:warning
"Arguments error ;\n   (set-transp o n a/2)\n   o octaves, n notename, a alterations")
(let((res (map
  (lambda(music)
    (music-map
      (lambda(m)
         (let ((p (ly:music-property m 'pitch #f)))
           (if p (ly:music-set-property! m 'pitch ;(begin
             ;(format #t "~a - " (ly:music-property m 'name))
             (ly:pitch-transpose p (delta p))));)
           m))
      music))
  (map ly:music-deep-copy
       (obj->music-list               ; a list of musics
         (flat-lst obj obj-args)))))) ; a big list of all instruments
; (if (= (length res) 1) (car res) res)))
  (if (or (list? obj)(pair? obj-args))
    res
    (car res)))))

% As I use a lot (set-transp n 0 0), I have added 2 functions : octave,
% and octavize. See also octave+ et add-note-octave
#(define (octave n obj)
"Short-cut for ((set-transp n 0 0) obj)"
(if (pair? obj)
  (map (lambda(x)(octave n x)) obj)
  ((set-transp n 0 0) (obj->music obj))))

#(define ((set-octave n) obj)
   (octave n obj))

#(define octave (int-music-generic octave))

#(define (octavize n obj from-pos to-pos . pos-args)
"Transpose the section [from-pos to-pos] by n octaves.
You can specify several sections by the following way :
 (octavize n obj pos1 pos2 / pos3 pos4 / pos5 pos6 etc...)"
(apply x-apply-to obj (set-transp n 0 0) from-pos to-pos pos-args))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% voices %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define (voice n music)
"Extract the n-th Voice in a multiple Voices music"
(extract-voice (ly:music-deep-copy music) n #f))

#(define ((set-voice n) music)
   (voice n music))

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
"Combine `voice with `obj at `where-pos.
`obj is a symbol of an instrument or a list of symbols.
`voice is a music or a list of musics.
Use `voice-start-pos, if `voice begins before `where-pos.
Use `to-pos if you want to stop before the end of `voice.
Use `obj-start-pos if 'obj doesn't begin at the beginning of the whole music.
`combine-func is a function with 2 music parameters (like `split or `chords) :
  if 'nthvoice is 1, param1 = voice, param2 = obj
  otherwise param1 = obj, param2 = voice "
(let*((end-pos (or to-pos 'end)) ;
      (voices (if (list? voice) voice (list voice)))
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
#(define (part-combine part1 part2)
;(make-part-combine-music (list part1 part2) #f)) ;; <- lilypond 2.18
; new in lilypond 2.20 : see ly/music-functions-init.ly ;
(make-directed-part-combine-music #f '(0 . 8) part1 part2
    #{ \with { \voiceOne \override DynamicLineSpanner.direction = #UP } #}
    #{ \with { \voiceTwo \override DynamicLineSpanner.direction = #DOWN } #}
    #{ #}))

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
"At measure `where-pos, assign to the first element of `instruments the first
voice of `music-with-voices, the second one to the second voice, and so on."
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
If other numbers are given, m, p ..., returns chords with all matching notes
If no note matches, returns the last note of the chord."
(let ((music (and (pair? args)(pair? (cdr args))
                  (obj->music (last args)))))
  (if (and music (ly:music? music))   ;; see chordsAndVoices.ly for extract-note
    (apply extract-note (ly:music-deep-copy music) args);(args is filtered)
    (ly:error "Bad syntax for procedure note.\n\t (note n [m p ...] music)"))))

#(define ((set-note . args) music)
(apply extract-note (ly:music-deep-copy music) args)) % (args is filtered)

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

% voices->chords behaves as \partcombine with \partcombineChords option
% To use with apply-to
#(define (voices->chords music)
"Transformes 2 simultaneous voices { a b } { c d } in { <a c> <b d> }"
((set-notes+ (voice 2 music))(voice 1 music)))

#(define (chords->voices music)
"Split 2 notes chords in 2 voices.
    <a c> <b d> < c e> becomes << { a b c} \\\\ { c d e } >> "
(split (note 2 music)(note 1 music)))

#(define (chords->nmusics n music)
"{<a c e> <b d g> <c e g c>} and n=3 will give the list ({e g g}{c d e}{a b c})"
(if (> n 0)
  (map (lambda(i)(note (1+ i) music))
       (reverse (iota n)))
  music))
#(define ((set-chords->nmusics n) music)
   (chords->nmusics n music))

#(define (octave+ n music)
(cond ((= n 0) music)
      ((pair? music)(map (lambda(m)(octave+ n m)) music))
      (else    ; ↱ force resolution pitch of { c4 2 8 } for ex
(let((music (expand-repeat-notes! (obj->music music))))
  (add-note   ; see chordsAndVoices.ly
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
  (for-each
    (lambda (instru i)
       (apply rm instru where-pos (note i music) args))
    instruments
    (iota i i -1) ; ioata count start step : '(i i-1 ... 1)
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
%%  if no self-alignment-X needed : f##-1.5 or f##2#-1.5

#(use-modules (ice-9 regex))

% Util function for add-dynamics
#(define (str->pos-dyn-list pos-dyn-str)
"Parse the string pos-dyn-str and return a list of string pair representing a 
position and a dynamic"
  (define (split-and-trim s char)
    (map string-trim-both (string-split s char)))
  (define (pos-str->mom pos-str)
    (eval-string (string-append "(pos->moment " pos-str ")")))
              ;;;;;;;;;;;;;;;;;;;;;
(reverse
  (fold
    (lambda (str prev-res)
     (let* ((len (string-length str))
            (last-par-i (string-rindex str #\)))    ;; last close parenthesis ) index (false is not found)
            (pos-end (or (and last-par-i (1+ last-par-i)) ;; include last )
                         (string-index str #\space) ;; first space
                         len))
            (pos-start (or (and last-par-i (string-index str #\()) ;; 1st open parenthesis (skipping ' char if any)
                           0))
            (pos (let ((s (substring str pos-start pos-end))) ; a string representing a position
               (cond ((not last-par-i)                        ; pas de parenthèses ( ... ) ?
                        (if (string-any char-set:letter s)    ; is s containing a least a letter ?
                          (pos->string (eval-string s))       ; must be a user-defined pos: A for example
                          s))        ; only digit or perhaps +- , hoping no extra unexpected characters...
;                          (number->string (eval-string s))) ;; ← allows (+ 57 29) or (+ A 29) => 86 (if A = 57)
                      (else   ;; it will allow (A 2 8) => '(57 2 8), or ((+ A 29) 2 8) => '(86 2 8)
                        (let ((s-splitted (string-split (substring s 1) #\space)))
                          ;(display (car s-splitted))
                          (if (let ((1st-word (car s-splitted)))                ; 1st word in s
                                (and (not (char=? (string-ref 1st-word 0) #\()) ; first char = "(" => it a sub list or func
                                     (procedure? (eval-string 1st-word))))      ; is it a func ? (must return a pos !)
                            (pos->string (eval-string s))                       ; apply func. Convert the resulting pos to string
                            (pos->string (eval-string (string-append "(list " (substring s 1))))))))))) ; force evaluation
        ; (format #t "\n~a - ~a" str pos)   ; ← uncomment for debugging
        (if (= pos-end len)                           ; ← if no dyn specified :
          (let ((mom (pos-str->mom pos)))             ; that means that user want
            (let loop ((source prev-res)              ; to delete a previous pos-str
                       (dest '()))                    ; (same pos but dyn not empty)
                    (cond ((null? source)(reverse dest))
                          ((equal? mom (pos-str->mom (caar source))) ; if the same moment is found
                             (append (reverse dest)(cdr source)))    ; skip this element
                          (else (loop (cdr source) (cons (car source) dest)))))) ; otherwise, keep it
                                  ;; ↓ if dyn not empty, (<> pos-end len) :
          (let* ((dyn (string-trim (substring str pos-end len)))  ; skip pos
                 (final-dyn (fold       ; formatage of dynamics :
                   (lambda(s prev-str) ; s can be in a form like : mf or ^mf or mf#0.5 or mf#0.5#-2 or mf#0.5#1#-2
                     (let* ((splitted-s (string-split s #\#))    ; #\# : split character = #
                            (safe-list (append splitted-s (circular-list #f)))
                            (1st (first safe-list))  ; mf or ^mf
                            (2nd (second safe-list)) ; 0.5...or #f
                            (3rd (third safe-list))  ; -2 (if 4th=#f) or 1 or #f
                            (4th (fourth safe-list)) ; -2 or #f
                            (dynstr (let ((s2 (substring 1st 0 1)))  ; s2 = string with 1st char
                                      (if (string-contains "-^_" s2)
                                        (string-append s2 "\\" (substring 1st 1)) ; => ^\mf
                                        (string-append  "\\" 1st))))              ; => \mf
                            (align-X-tweak (if (and 2nd (not (string-null? 2nd)))
                                       (string-append "-\\tweak self-alignment-X " 2nd " ")
                                       ""))
                            (offset-tweak (let ((Y-offset (or 4th 3rd))
                                                (X-offset (or (and 4th 3rd) "0")))
                                            (if Y-offset
                                 (begin
                                    (if (string-null? Y-offset) (set! Y-offset "0"))
                                    (string-append "-\\tweak extra-offset #'(" X-offset " . "  Y-offset " )"))
                                  ""))))
                       (string-append prev-str align-X-tweak offset-tweak dynstr)))
                    ""
                    (split-and-trim dyn #\space)))) ; mf cresc =>
                                                    ; s = mf then s = cresc
                (acons pos final-dyn prev-res)))))
         ;  (begin
;             (ly:warning (_ "String not valid for add-dynamics :\n  \"~a\" !") str)
;             prev-res))))
    '()
    (split-and-trim pos-dyn-str #\/)) ; split by slash /
  ))

% (add-dynamics 'sym "5 mf / 9 _p cresc / (15 4) !"
% will give :
% (rm-with 'sym 5 #{ <>\mf #} / 9 #{ <>_\p\cresc #} / '(15 4) #{ <>\! #}
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
  (let ((res-str (fold
          (lambda (pos-dyn prev-str)
            (string-append prev-str " " (car pos-dyn) " #{ <>" (cdr pos-dyn) " #}"))
          ""
          (str->pos-dyn-list pos-dyn-str))))
    ;(format #t "****** position music :\n   ~a\n" res-str)

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

#(define (assoc-pos-dyn . str-obj) ;; string1 obj1 / string2 obj2 ....
(let loop ((l (filter not-procedure? str-obj)) ;; skip /
           (res '()))
  (if (or (null? l)(null? (cdr l)))
    (reverse res)
    (loop (cddr l)                           ;; next str obj
          (acons (car l)                     ;; str
                 (flat-lst (obj->instru-list (cadr l))) ;; obj (a list of instruments)
                 res)))))				     %% res = '(str . obj) + res

                %%%%%%%%%%%%%%%%%%%%%%%%
                %%% extract-dyn-str  %%%

% xor is not defined in standard guile 1.8. We don't use this function directly
% but something called 'xor has to be defined
 #(if (not (defined? 'xor)) (define (xor a b) (and a (not b))))

#(define (extract-pos-dyn-str extract-code assoc-dyn-list)
  (define (clean-code elts)  ;; make a list with only symbols and sub-lists :
    (fold-right              ;; a sub-list will have at least 1 operator and 1 elt
      (lambda(x prev-lst)
        (cond
          ((instrument? x)(cons x prev-lst))  ;; keeps instrument? also 'and 'or 'xor
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
         (lambda(anchor prev-list) ; anchor entry = '(moment . sym) or '(moment . syms)
           (let ((mom1 (car anchor))
                 (mom2 (car prev-list))
                 (syms (cdr anchor)))
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
       (seq (map delete-anchors  ; cleaning
                 (cdr res))))    ; skip mom1
  (if (= (length seq) 1)
     (car seq)
     (make-sequential-music seq))))

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
#(define-markup-command (note=note layout props note1 note2 open-par close-par)
                                               (string? string? markup? markup?)
(let((note1-without-stem #{ \markup \note #note1 #0 #}) ; avoid center alignment pbs
     (note2-without-stem #{ \markup \note #note2 #0 #})) ; #0 => no stem
  (interpret-markup layout props
    #{ \markup \concat \smaller \raise #-0.1 \general-align #Y #DOWN {
         #open-par
         \smaller \general-align #Y #DOWN
           \with-dimensions-from #note1-without-stem \note #note1 #1
         \hspace #0.3 "=" \hspace #0.1
         \smaller \general-align #Y #DOWN
           \with-dimensions-from #note2-without-stem \note #note2 #1
        #close-par
     } #})))

#(define-markup-command (note=tempo layout props note tempo open-par close-par)
                                                 (string? string? markup? markup?)
(let((note-without-stem #{ \markup \note #note #0 #}))
  (interpret-markup layout props
    #{ \markup \smaller \raise #-0.1 \general-align #Y #DOWN \concat  {
         #open-par
         \smaller \general-align #Y #DOWN
           \with-dimensions-from #note-without-stem \note #note #1
         \hspace #0.3 "=" \hspace #0.1
         #tempo
         #close-par
    } #})))

#(define* (metronome mvt note arg #:optional (txt "") open-par close-par)
"Return a markup like \\tempo does. `mvt and `note are string.
 If `arg is a number, `arg is the number of ticks per minute, else it is like the `note
 parameter : a string reprensenting a rhythm of a note : \"4.\" for ex.
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

% #(define (tempos where-pos txt . pos-str)
% (apply rm-with 'global
%          where-pos #{ \tempo $txt #}
%          (map (lambda(x)
%                 (if (markup? x) #{ \tempo $x #} x))
%               pos-str)))

#(define (tempos obj where-pos txt . args)
"Syntax : tempos obj where-pos1 txt1 [space1] / where-pos2 txt2 [space2] / ...
Add a command : \tempo txt, at where-pos in global. If space is specified,
move horizontaly by space units, the tempo markup"
(let loop ((res '()) ; a list of list
           (elt '()) ; a sub list of res
           (l (reverse              ; all arguments reversed
                (cons where-pos     ; l will end by a pos preceeded by...
                      (cons txt     ; a markup
                            (filter not-procedure? args)))))) ; skip /
  (if (pair? l)
    (let ((arg (car l))
          (next (cdr l)))
      (if (markup? arg)                   ; arg is txt argument ?
        (loop (cons (cons (car next)      ; yes ? where-pos is just after. (reversed list)
                          (cons arg elt)) ; add where-pos and txt to elt.
                    res)                  ; add elt to res
              '()                         ; reset elt
              (cdr next))                 ; skip the 2 elements added
        (loop res
              (cons arg elt)              ; just add arg to elt
              next)))
    (map (lambda(entry)                             ; entry = '(where-pos txt [space])
           (rm obj (first entry)                    ; where-pos
             (if (< (length entry) 3)               ; no 3rd element (space argument) ?
               #{ \tempo \markup #(second entry) #} ; txt.
               #{ \tempo \markup { \hspace #(third entry) #(second entry) } #})))
         res))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% working with patterns %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% rhythm patterns : see changePitch.ly
#(define ((set-pat pattern . args) obj)
"Apply `change-pitch `pattern to `obj. Return a list if `obj is a list, or a
music otherwise. If `pattern ends with rests, you can add #t as last args
parameter to keep these rests at the end."
(let ((func (lambda(x)
        (let* ((pat (expand-q-chords (ly:music-deep-copy pattern)))
               (music (expand-q-chords (ly:music-deep-copy (obj->music x))))
               (notes (if (and (pair? args)(car args))  ; if args is #t
                 (seq music (moment->skip ZERO-MOMENT)) ; adds a \skip
                 music)))
          (change-pitch pat notes)))))
  (if (list? obj)(map func obj)(func obj))))

#(define ((set-pitch from-notes) obj)
"Change pitches of obj notes by pitches of `from-notes notes. 
Rhythm is untouched, articulations mixed."
(let ((func (lambda(x)
        (let ((pat (expand-q-chords (ly:music-deep-copy (obj->music x))))
              (notes (expand-q-chords (ly:music-deep-copy from-notes))))
          (change-pitch pat notes)))))
  (if (list? obj)(map func obj)(func obj))))


#(define (cp . args)
(case (length args)
  ((3) ((set-pat (second args)(first args)) (third args)))
  ((2) ((set-pat (first args) #t) (second args)))
  (else (ly:error "bad syntax : (cp [keep-last-rests?] pattern music) !"))))

#(define-macro (cp1 obj) `(cp patI ,obj))
#(define-macro (cp2 obj) `(cp patII ,obj))

#(define (cp-with obj pos new-notes . others-pos-new-notes)
(let loop ((pos pos)
           (new-notes new-notes)
           (args (filter
              (lambda(x)(or (not-procedure? x)(promise? x)))
              others-pos-new-notes)))
   (apply-to obj (set-pitch new-notes) pos 'end)
   (if (>= (length args) 2)(loop (first args)(second args)(list-tail args 2)))))


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
               (loop2 (cdr l2)(cons (if func (func (list-ref l1 (1- n)))
                                             (list-ref l1 (1- n)))
                                    res2))
               (make-sequential-music (reverse res2))))))))))

#(define ((set-tweak-notes-seq n-list) mus)
  (tweak-notes-seq n-list mus))

%%% articulation patterns
#(define ((set-arti pattern) obj)  ; see copyArticulations.ly
   (define (music-func mus) (copy-articulations pattern mus))
((music-func->obj-func music-func) obj))

#(define (ca pattern obj)
((set-arti pattern) obj))

% #(define ((set-del-events event-sym . args) obj)
% "Delete all events named `event-sym. Several events can be specified,
% or even a list of event."
% (let ((events-list (flat-lst event-sym args)))
%   (define (func obj)
%      (if (list? obj)
%        (map func obj)
%        (music-filter
%          (lambda (evt)
%             (not (memq (ly:music-property evt 'name) events-list)))
%          (ly:music-deep-copy (obj->music obj)))))
%   (func obj)))

#(define ((set-del-events event-sym . args) obj)
"Delete all events named `event-sym. Several events can be specified,
or even a list of event."
(let ((events-list (flat-lst event-sym args)))
  (define (music-func mus)
     (music-filter
       (lambda (evt)
         (not (memq (ly:music-property evt 'name) events-list)))
     mus))
  ((music-func->obj-func music-func) obj)))

%%% music patterns
#(define (fill-with pat from-pos to-pos)
"Repeats the pattern `pat, as much times needed to fill the range [from-pos 
to-pos], cutting if necessary the last repeat to fit exactly the range."
(if (list? pat)
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
            (make-music 'PercentRepeatedMusic
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
  (if (list? alignAboveContext) alignAboveContext (circular-list alignAboveContext))))

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
      (show-infos (pred->arg boolean? #f)))
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
%% display-lily-music add a line break only when a bar-check is found.
%% This function try to add a "|" for-each measure (em music i (1+ i)) but
%% multi-measure rests must not be splitted. Idem for
%% simultaneous Music. So the following function :
#(define* (add-bar-checks music #:optional (music-begin-pos (moment->pos ZERO-MOMENT)))
   (define (make-has-proc named-music-list named-music-sym)
     (lambda(m) (and
       (pair? named-music-list)
       (pair? (extract-named-music m named-music-sym)))))
(let*((music ((set-del-events 'BarCheck) music)) ;; del BarChecks to avoid double Barcheck
       ; list of measure-rest
      (MMRs (extract-named-music music 'MultiMeasureRestMusic))
      ; SIMs (Simultaneous) : list of 3 elts vectors #(sim-music mom-start mom-end)
      (SIMs (reverse
        (let((mom (pos->moment music-begin-pos)))
          (let loop ((m music)
                     (res '()))
            (let ((name (name-of m))
                  (dur? (ly:music-property m 'duration #f)))
              (cond
                ((or dur? (eq? name 'EventChord))
                   (set! mom (ly:moment-add mom (ly:music-length m)))
                   res)
                ((eq? name 'SimultaneousMusic)
                   (let ((mom-start mom)
                         (mom-end (ly:moment-add mom (ly:music-length m))))
                     (set! mom mom-end)
                     (cons (vector m mom-start mom-end) res)))
                (else
                   (fold
                     loop
                     (let ((elt (ly:music-property m 'element)))
                        (if (null? elt) res
                                        (loop elt res)))
                     (ly:music-property m 'elements)))))))))
      (has-MMR? (make-has-proc MMRs 'MultiMeasureRestMusic))
      (has-SIM? (make-has-proc SIMs 'SimultaneousMusic))
      (1st-i (pos:num music-begin-pos))
      (partial (and (= 1st-i first-measure-number)
                    (extract music ZERO-MOMENT (pos->moment first-measure-number)))))
  ;   (for-each (lambda(v) (format #t "start-pos ~a : end-pos ~a\n" (vector-ref v 1)(vector-ref v 2))
;                           (display-lily-music (vector-ref v 0)))
;               SIMs)
  (let loop ((i 1st-i) ; the measure number
             (res (if (and partial (defined-music? partial)) ; if partial...
                    (list (seq partial (make-music 'BarCheck)))        ; ... add it.
                    '())))
    (let ((m (em music i (1+ i) music-begin-pos)))  ;; extract 1 measure
      (if (defined-music? m) ;; true while end not reached
        (loop (cond ;; find next i
                ((has-MMR? m) ; avoid for ex : R1*3 to be splitted in : R1 R1 R1
                   (let* ((mmr (car MMRs))
                          (end-mmr-pos (moment->pos
                               (ly:moment-add (pos->moment i) (ly:music-length mmr))))
                          (new-i (pos:num end-mmr-pos)))
                     ; (format #t "~a ~a\n" i new-i)
                     (set! m (em music i new-i music-begin-pos))
                     (set! MMRs (cdr MMRs))
                     new-i)) ;; measure number after mmr
                ((has-SIM? m)
                    (let* ((entry (car SIMs))
                           (sim (vector-ref entry 0))
                           (start-pos (moment->pos (vector-ref entry 1)))
                           (end-pos (moment->pos (vector-ref entry 2)))
                           (new-i (if (equal? (pos:remain end-pos) ZERO-MOMENT) ;; 1st beat ?
                                    (pos:num end-pos)
                                    (1+ (pos:num end-pos)))))
                    ; (format #t "start : ~a - end : ~a\n" start-pos end-pos)
                     (ly:music-set-property! sim 'elements (map
                       (lambda(e)(add-bar-checks e start-pos))
                       (ly:music-property sim 'elements)))
                     (let ((es (filter defined-music?
                                       (list (em music i start-pos music-begin-pos)
                                             sim
                                             (em music end-pos new-i music-begin-pos)))))
                        (if (pair? es)
                          (set! m (cond ((pair? (cdr es)) (apply seq es))
                                        (else (car es))))))
                     (set! SIMs (cdr SIMs))
                     new-i)) ;; measure number after m
                 ;; default next i
                (else (1+ i)))
              ;; res
              (if (equal? (ly:music-length m) ZERO-MOMENT)
                (cons m res)
                (cons (seq m (make-music 'BarCheck))
                      res)))
        ;; not defined-music? => end of music reached
        ;(display i)
        (cond ((null? res) (make-music 'Music))
              ((null? (cdr res)) (car res))
              (else (apply seq (reverse res)))))))))
%        (reduce-seq (make-sequential-music (reverse res))))))))


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
              "                 %%%%%% instruments export du ~a %%%%%%\n\n"
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
