\version "2.20.0"
%% version Y/M/D = 2020/07/04: for lilypond 2.20
%% LSR = http://lsr.di.unimi.it/LSR/Item?id=542
% Last modif. (the last at the end) :
% - change append! by cons in (make-signature-list)
% - replace moment-null definition, by ZERO-MOMENT, which has been
%   already defined in lily-library.scm.Delete also moment-min definition.
% - Add better support for some special moment value in moment->rhythm.
%   measure-number->moment returns a correct value if \global has an incomplete
%   measure.
% - Changes in (make-signature-list) for \partial and Timing.measurePosition
% - fixes in (make-signature-list) a problem in measure-number->moment when
%   the last measure of \global was incomplete
% - \tuplet have now a 'duration property. (This property is no more reserved for
%   notes, rests, and skips) => we have to change line 174
% - \cadenzaOn/Off compatibility
% - allows \displayLilyMusic to work, using reduce-seq in a music with a \tempo command
% - \replaceVoltaMusic was buggy for music out of volta structure : Rewriting.

#(define (expand-q-chords music); for q chords : see chord-repetition-init.ly
 (expand-repeat-chords! (list 'rhythmic-event) music))

%%%%%%%%% *current-moment* = a global parameter used by extractMusic %%%%%%%%%%%
%% see :     http://www.gnu.org/software/guile/manual/guile.html#SRFI_002d39

#(use-modules (srfi srfi-39))

#(define *current-moment* (make-parameter ZERO-MOMENT))

%%%%%%%%%%%%%%%%%% some little functions used by extract-range %%%%%%%%%%%%%%%%%
#(define (name-of music)
 (ly:music-property music 'name))

#(define (defined-music? music)
   (not (eq? 'Music (name-of music))))

% a moment<=? is defined in lily-library.scm, but i prefer to use this function
#(define (moment>=? a b)
  (not (ly:moment<? a b)))

% moment-min is defined in lily-library.scm but not moment-max.
#(define (moment-max a b)
    (if (ly:moment<? a b) b a))

#(define (whole-music-inside? begin-music end-music left-range right-range)
   (and (moment>=? begin-music left-range)
        (moment>=? right-range end-music )
        (not (equal? begin-music right-range)))) %% don't take 0-length events
                             %% (as \override for ex) beginning at right-range
                             %% (when begin-music = end-music = right-range)
#(define (whole-music-outside? begin-music end-music left-range right-range)
   (or (moment>=? left-range end-music)
       (moment>=? begin-music right-range)))

                  %%%%%%%%%  moment->rhythm  %%%%%%%%%
% see first duration.cc in Lilypond sources (Duration::Duration)
% and ly:make-duration in Lilypond doc
%% See also for a try for a mathematical explanation (... in french), here :
%%  http://gillesth.free.fr/Lilypond/extractMusic/more-doc/momentToRhythm-doc-fr.pdf
%% Tex Sources in : http://gillesth.free.fr/Lilypond/extractMusic/more-doc/
#(define (moment->rhythm moment)
"Try to convert moment to a duration suitable for displaying a note or a rest,
so in the following form : (ly:make-duration k dots 1 1)
Note that, if moment=5/8, for example, no duration of this form is possible."
(let* ((p (ly:moment-main-numerator moment))
       (q (ly:moment-main-denominator moment))
       (k (- (ly:intlog2 q) (ly:intlog2 p))))
 (if (< (ash p k) q) (set! k (1+ k))) ; (ash p k) = p * 2^k
 (if (> k 6)
   (ly:make-duration 6 0) ; 6 means 64th (max value).
   (let loop ((p1 (- (ash p k) q))
              (dots 0))
      (let ((p2 (ash p1 1)))
        ;; (format #t "p1 = ~a ; p2 = ~a\n" p1 p2)
        (if (>= p2 q)
          (loop (- p2 q) (1+ dots))
          ;; it seems that (not documented) :
          ;;    (ly:duration-length (ly:make-duration a b c d)) =
          ;;    (ly:moment-mul (ly:duration-length (ly:make-duration a b))
          ;;                   (ly:make-moment c d))
          (let* ((dur (ly:make-duration k dots))        ; duration for displaying the note
                 (dur-len (ly:duration-length dur))     ; his display length.
                 (frac (ly:moment-div moment dur-len))) ; to adjust to the real length
            (ly:make-duration k dots
              (ly:moment-main-numerator frac) ; frac = 1/1 for moment = 3/4, 7/8 etc ..
              (ly:moment-main-denominator frac)))))))))

%%%%%  Here are some macros, to keep the extract-range code more compact  %%%%%

#(define-macro (filter-elts-for-non-sequential-music);Chords, SimultaneousMusic
   '(filter defined-music? (map
        (lambda (evt)
          (let ((extracted-evt (extract-range evt from to)))
            (if (equal? (*current-moment*) begin-pos)
                evt ; keeps 0 length events such as scripts, or 'VoiceSeparator.
                (begin
                  (*current-moment* begin-pos) ;% restore *current-moment*
                  extracted-evt))))
         elts)))

#(define-macro (filter-elts-for-sequential-music) ; sequential music
  '(filter defined-music? (map
        (lambda (evt)
          (extract-range evt from to))
        elts)))

      %%%% a big macro for repeated-music %%%%%%%
%{ The extract-repeated-music macro deals with music having the following form :
(make-music 'name
    'elements elts
    'repeat-count n
    'element elt  %%  (make-music 'EventChord or make-sequential-music ...
%}

#(define-macro (extract-repeated-music) ; But not volta-music
'(if (not (pair? elts))
  (let* ((unfold-music (make-sequential-music (map
                         (lambda(section) (ly:music-deep-copy elt))
                         (make-list (ly:music-property music 'repeat-count)))))
         (extracted-sections (filter defined-music?
                                     (ly:music-property
                                       (extract-range unfold-music from to)
                                       'elements)))
         (count (length extracted-sections)))
      (case count
        ((0) (make-music 'Music))
        ((1) (car extracted-sections))
        (else   ; the 1st or the last sections has been perhaps shortened
          (let* ((first-section (car extracted-sections))
                 (last-section (car (last-pair extracted-sections))) ; use last instead ?
                 (seq-elts (list #f #f #f));(shortened? count*elt shortened?)
                 (elt-length (ly:music-length elt)))
            (if (ly:moment<? (ly:music-length first-section) elt-length)
                (begin
                  (set! count (1- count))
                  (list-set! seq-elts 0 first-section)))      ; 0 = first elt
            (if (ly:moment<? (ly:music-length last-section) elt-length)
                (begin
                  (set! count (1- count))
                  (list-set! seq-elts 2 last-section)))       ; 2 = 3rd elt
            (cond ((= count 1) (list-set! seq-elts 1 elt))    ; 1 = 2nd elt
                  ((> count 1) (list-set! seq-elts 1
                         (make-music name 'repeat-count count 'element elt))))
            (set! seq-elts (filter (lambda (x) x) seq-elts)) ;delete trailing #f
            (if (= (length seq-elts) 1)
                  (car seq-elts)
                  (make-sequential-music seq-elts))))))
  ; volta-repeat musics use 'element AND 'elements
  (let* ((extracted-elt (extract-range (ly:music-deep-copy elt) from to))
         (extracted-elts (filter defined-music? (map
                           (lambda (section)(extract-range section from to))
                           elts))))
    (cond ((not (defined-music? extracted-elt))
              (case (length extracted-elts)
                ((0) (make-music 'Music))
                ((1) (car extracted-elts))
                (else (make-sequential-music extracted-elts))))
          ((null? extracted-elts) extracted-elt)
          (else (make-sequential-music (cons extracted-elt extracted-elts)))))
))

                %%%%%% the extract-range function %%%%%%%
%%% This function cannot be used directly : *current-moment* has to be
%%% initialized before. You can get also some strange behaviour, if you
%%% don't use (ly:music-deep-copy). Use extract-during below, instead.
#(define (extract-range music from to)
"Keeps only music beetween `from and `to, `from and `to as moment"
(let ((begin-pos (*current-moment*))
      (end-pos (ly:moment-add (*current-moment*) (ly:music-length music))))
 (*current-moment* end-pos) ;for the next music to process
 (cond
  ((whole-music-inside? begin-pos end-pos from to) music)
  ((whole-music-outside? begin-pos end-pos from to)(make-music 'Music))
        ; From this point, the intervals [begin-pos end-pos][from to] overlap
  (else
    (let((name (ly:music-property music 'name)))
     (if (and (ly:duration? (ly:music-property music 'duration))
              (not (eq? name 'TimeScaledMusic))) ; tuplet have a duration now !
       (begin    ; a NoteEvent, a skip, a rest, a multiRest
         (set! begin-pos (moment-max begin-pos from))
         (set! end-pos (moment-min end-pos to))
         (ly:music-set-property! music 'duration
            (if (memq name (list 'NoteEvent 'RestEvent))
              (moment->rhythm (ly:moment-sub end-pos begin-pos))
              (make-duration-of-length (ly:moment-sub end-pos begin-pos)))))
                ; for containers of duration evt, or a chord
       (let ((elts (ly:music-property music 'elements))
             (elt  (ly:music-property music 'element)))
          (*current-moment* begin-pos)    ;we go deeper into the same music evt
          (cond
            ((string-contains (symbol->string name) "RepeatedMusic")
               (if (eq? name 'VoltaRepeatedMusic)
                 (set! music (extract-range (make-sequential-music (cons elt elts)) from to))
                 (set! music (extract-repeated-music)))) ; other repeated-musics see macros above
            ((ly:music? elt)(ly:music-set-property! music 'element
                                          (extract-range elt from to)))
            ((pair? elts)
              (let ((new-elts (if (memq name (list 'SimultaneousMusic 'EventChord))
                      (filter-elts-for-non-sequential-music) ;; see macros
                      (filter-elts-for-sequential-music))))
                (if (null? new-elts)
                  (set! music (make-music 'Music))
                  (ly:music-set-property! music 'elements new-elts)))))
          (*current-moment* end-pos))) ; next music evt
     music)))))

%% Before defining the music-function \extractMusic, we define a helpful
%% function \upToMeasure, to let the user define the `from and `during
%% parameter of \extractMusic, using the number of the measure.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% upToMeasure %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% \extractMusic works only with moments (as ly:music-length does), but measures numbering
%% implies to care of all timing events. Dealing with signature events is not too difficult but
%% the tricky part is to deal with \partial, \cadenzaOn/Off, and all others timing adjustements.
%% We make a list of timing sections in a global variable called *timing-sections* which make
%% possible the conversion of a measure number to a moment.

%% moment utilities
#(define infinite-number 1000000000) % quasi infinite :-)
#(define infinite-mom (ly:make-moment infinite-number))
#(define infinite-mmR (make-music 'MultiMeasureRestMusic 'duration (ly:make-duration 0 0 infinite-number)))
% WARNING : to use only within extractMusic functions. (See the \mmR definition at the bottom of this file)
% A music just like { \compressFullBarRests #infinite-mmR } take 5 mn to compile on my machine

#(define (mom->integer mom) (quotient (ly:moment-main-numerator mom)
                                      (ly:moment-main-denominator mom)))
#(define (mom-add mom . rest) (fold ly:moment-add mom rest))     % multiple args
%#(define (mom-sub mom . rest) (fold ly:moment-sub mom rest))    % <- no : parameters in proc are in wrong order
%#(define (mom-sub mom . rest) (fold (lambda(current prev)(ly:moment-sub prev current)) mom res)) % parameters in right  order
#(define mom-sub ly:moment-sub) % i don't really need a multiple args version here, but a short-cut is fine
#(define mom-div ly:moment-div) % an other short-cut
#(define (mom-imul mom n) (ly:moment-mul mom (ly:make-moment n))) % multiplication by integer
#(define (mom-delta-div from to len) (ly:moment-div (ly:moment-sub to from) len)) % len count between from to
#(define (mom-delta-mod from to len) (ly:moment-mod ; <- the remaining after division
                                       (if (ly:moment<? ZERO-MOMENT len) (ly:moment-sub to from)(ly:moment-sub from to))
                                       len)) % => result will be < 0 if len < 0. Needed for partial at start
%% make-timing-sections : use to initialize the *timing-sections* variable or by measure-number->moment
#(define (make-timing-sections first-measure)
"Make a list of vectors in the form : #(n mom 1measure-len)  
n is the measure number of the beginning of a new timing section. 
mom is the corresponding moment position.
1measure-len is the length of 1 measure in this section (a moment).
The list is returned in reverse order, the latest section at beginning. 
All signatures or manual timing changes have to be set by the user in a music variable 
named \\global. The first event in global begins always at measure first-measure, at ZERO-MOMENT"
;; we collect all timing infos (signatures partials etc...) in a list called timings
(let ((timings '())                     ; the list to build
      (local-pos ZERO-MOMENT))          ; current moment pos
    (define (add-timing mom sym value)  ; each elt of timings is a 3 elts vector : #(moment sym value))
      (set! timings (cons (vector mom sym value) timings)))
    (define (collect-timing-infos evt)   ;  big inner-function
      (let ((name (ly:music-property evt 'name))
            (dur (ly:music-property evt 'duration #f)))
         (cond
           ((eq? name 'TimeSignatureMusic);; a time signature change occurs
              (add-timing local-pos name
                        (ly:make-moment (ly:music-property evt 'numerator)
                                        (ly:music-property evt 'denominator))))
           ((or dur (eq? name 'EventChord))      ;; notes chords skips rests etc...
              (set! local-pos (ly:moment-add local-pos (ly:music-length evt))))
           ;((and (eq? name 'ContextSpeccedMusic)           ;; override of Timing evts, but...
           ;      (eq? (ly:music-property evt 'context-type) 'Timing)) ; not only,can be Score
           ((eq? name 'ContextSpeccedMusic) ;; \override
              (let loop ((ctx-elt (ly:music-property evt 'element))) ;; some event have 2 levels depth
                (if (ly:music? ctx-elt)
                  (let ((name (ly:music-property ctx-elt 'name)))    ;; can be Score, Bottom, ... or Timing
                    (case name
                      ((PartialSet)  ;; \partial dur music
                         (let* ((dur (ly:music-property ctx-elt 'duration #f))
                                (offset (if dur (ly:duration-length dur) ZERO-MOMENT)))
                           (add-timing local-pos name offset)))
                      ((PropertySet)
                         (let ((sym (ly:music-property ctx-elt 'symbol))
                               (val (ly:music-property ctx-elt 'value)))
                           (if (memq sym '(timing measurePosition measureLength currentBarNumber))
                             (add-timing local-pos sym val))))
                      (else (loop (ly:music-property ctx-elt 'element)))))))) ;; 2nd level for partial, currentBarNumber
           (else
            (let ((elt (ly:music-property evt 'element))
                  (elts (ly:music-property evt 'elements))
                  (count (ly:music-property evt 'repeat-count)))
              (cond          ;all repeated-music but volta-repeats
                ((and (integer? count)
                      (null? elts)
                      (not (eq? name 'VoltaRepeatedMusic)))
                   (collect-timing-infos
                     (make-sequential-music (make-list count elt))))
                ((eq? name 'SimultaneousMusic)
                   (let ((save-pos local-pos)
                         (max-pos local-pos))
                     (for-each
                       (lambda(x)
                         (collect-timing-infos x)
                         (if (ly:moment<? max-pos local-pos)
                           (set! max-pos local-pos))
                         (set! local-pos save-pos))
                         elts)
                     (set! local-pos max-pos)))
                (else    ; all sequential-musics, volta-repeats etc ...
                  (if (ly:music? elt) (collect-timing-infos elt))
                  (if (pair? elts) (for-each collect-timing-infos elts)))))))))
     ;; End of collect-timing-infos definition. Call it now to build timings from \global
     ;; At that point, all timing infos are still in reverse order (the last at the beginning)
   (collect-timing-infos (ly:music-deep-copy global))
     ;; Before reverting the list, we add a marker : a very big moment that will be used as a end-marker
   (add-timing infinite-mom 'end-of-list ZERO-MOMENT)
     ;; instead of reverting, we sort the list (needed if global has simultaneous music)
   (let((less (lambda(info1 info2) ; the smaller mom, if moms equal, timeSignature first
                (if (eq? 'TimeSignatureMusic (vector-ref info1 1))
                  (moment>=? (vector-ref info2 0) (vector-ref info1 0))       ; info1 <= info2 [(moment>= b a) = (ly:moment<= a b)]
                  (ly:moment<? (vector-ref info1 0) (vector-ref info2 0)))))) ; info1 < info2
     (set! timings (sort timings less)))
    ;(display timings)
;; Construction of sections, the list to return. Each element (a section) will be represented
;; by a 3 elts vector : index 0 = start of the section as a measure number
                     ;; index 1 = start of the section as a moment
                     ;; index 2 = len of 1 measure in this section
;; Sorry, here we break indentation !!!  (need space at right side.)
(let ((cadenzaOn-infos #f) ; for cadenzaOn/Off evts (see end of loop)
      (section1 (let ((first-timing (car timings))) ; the first signature is optionnal in Lilypond
                  (if (and (equal? ZERO-MOMENT (vector-ref first-timing 0))
                           (eq? 'TimeSignatureMusic (vector-ref first-timing 1)))
                    (begin
                      (set! timings (cdr timings)) ; skip first-timing
                      (vector first-measure ZERO-MOMENT (vector-ref first-timing 2)))
                    (vector first-measure ZERO-MOMENT (ly:make-moment 4/4)))))) ; default signature
 (let loop ((sections (list section1))   ; add the first entry to sections
            (timings timings))
   (let* ((timing-evt (car timings))
          (mom (vector-ref timing-evt 0))
          (sym (vector-ref timing-evt 1))
          (val (vector-ref timing-evt 2))
          (prev-entry (car sections))
          (n (vector-ref prev-entry 0))
          (prev-mom (vector-ref prev-entry 1))
          (1measure-len (vector-ref prev-entry 2))
          (delta-n (mom->integer (mom-delta-div prev-mom mom 1measure-len)))) ; section measures count
     (case sym
       ((end-of-list) sections) ; return
       ((TimeSignatureMusic)
          (loop (cons (vector (+ n delta-n) mom val)
                  (if (equal? mom prev-mom) (cdr sections) ; when prev measure has a \partial
                                            sections))
                (cdr timings)))
       ((PartialSet)   ; \partial behaves differently at the beginning or inside music (see Lily doc).
          (if (equal? mom ZERO-MOMENT)           ; mom = 0
            (loop (cons (vector first-measure val 1measure-len)            ; measure 1 : with measure len > 0
                    (cons (vector first-measure ZERO-MOMENT (mom-sub ZERO-MOMENT val)) ; with measure len < 0
                      (cdr sections)))  ; delete prev entry
                  (cdr timings))
            (let* ((partial-n (+ n delta-n))     ; mom <> 0
                   (next-partial-n (1+ partial-n))
                   (partial-mom (mom-add prev-mom  ; prev-mom + ((partial-n - 1) x 1measure-len)
                                         (mom-imul 1measure-len (1- partial-n))))
                   (next-partial-mom (mom-add mom val))
                   (partial-len (mom-sub next-partial-mom partial-mom)))
              (loop (cons (vector next-partial-n next-partial-mom 1measure-len) ; restore the prev partial
                      (cons (vector partial-n partial-mom partial-len)          ; add partial measure
                        sections))
                    (cdr timings)))))
       ((measurePosition)
          (loop (cons (vector (+ n delta-n 1)
                              (mom-add ; next = next-without-mP + (pos-in-mes-without-mP - pos-in-mes-with-mP)
                                prev-mom
                                (mom-imul 1measure-len (1+ delta-n)) ; next mP measure without mP adjustement.
                                (mom-sub (mom-delta-mod prev-mom mom 1measure-len) val)) ; r - val : delta = mom/1measure-len + r
                              1measure-len) sections)
                (cdr timings)))
       ((measureLength) (loop (cons (vector (+ n delta-n) mom val)
                                (if (equal? mom prev-mom) (cdr sections) sections))
                              (cdr timings)))
       ((currentBarNumber)(loop (cons (vector (+ val first-measure -1) mom 1measure-len)
                                  (if (equal? mom prev-mom) (cdr sections) sections))
                                (cdr timings)))
       ((timing) ; ; val #t / #f => cadenzaOff / On
          (if val ; if cadenza return to Off : retrieve infos set in cadenzaOn-infos while cadenzaOn
            (if cadenzaOn-infos ; a 3 elts vector
              (let* ((cadenzaOn-mom (vector-ref cadenzaOn-infos 0))    ; evt cadenzaOn start
                     (n (vector-ref cadenzaOn-infos 1))                ; cadenza measure number
                     (measure-Left-mom (vector-ref cadenzaOn-infos 2)) ; the corresponding mom : measure start
                     (delta-OnOff (mom-sub mom cadenzaOn-mom))         ; distance between : cadenzaOn / cadenzaOff evts
                     (delta-LeftOn (mom-sub cadenzaOn-mom measure-Left-mom)) ; distance : start of measure / cadenzaOn evt
                     (delta-OffRight (mom-sub 1measure-len delta-LeftOn))    ; distance : cadenzaOff evt / end of measure
                     (measure-len (mom-add delta-LeftOn delta-OnOff delta-OffRight))
                     (measure-Right-mom (mom-add measure-Left-mom measure-len)))
                ;(format #t "mom : ~a, cadenzaOn-mom : ~a\n" mom cadenzaOn-mom)
                ;(format #t "delta-OnOff : ~a, delta-LeftOn : ~a\n" delta-OnOff delta-LeftOn)
                (let loop2 ((entry-mom prev-mom)) ; first, delete all entries inside cadenza
                  (if (moment>=? entry-mom cadenzaOn-mom)
                    (begin (set! sections (cdr sections))
                           (loop2 (vector-ref (car sections) 1)))))
                (set! cadenzaOn-infos #f)
                (loop (cons (vector (1+ n) measure-Right-mom 1measure-len)
                        (cons (vector n measure-Left-mom measure-len)
                          sections))
                      (cdr timings)))
              (loop sections (cdr timings))) ; cadenzaOn-infos = #f : a cadenzaOff but no cadenzaOn !!! => do nothing
            (begin ;; a cadenzaOn begins. Store infos in cadenzaOn-infos
              (set! cadenzaOn-infos (vector mom
                                            (+ delta-n n)  ; cadenza measure number
                                            (mom-add prev-mom (mom-imul 1measure-len delta-n)))) ; start of measure
              (loop sections (cdr timings)))))
              ))))))

%% (*timing-sections*) is a global variable. For now we don't really need the make-parameter but it can
%% perhaps enlarge the user possibilities with the restoring abilities of his parameterize function.
#(define *timing-sections* (make-parameter #f))

%% measure-number->moment (used by \upToMeasure)
%% The caller as the possibility to re-build at each call the timing sections list or to
%% save it once in the *timing-sections* variable. This possibility is used in arranger.ly
%% to allow user to add timing events in \global with build-in functions, while keeping in
%% "real-time", the right correspondance : measure numbers / moments
%% Te optional arg first-measure is used also in arranger.ly
%% It is the caller responsability to check if number >= first-measure
#(define* (measure-number->moment number #:optional first-measure)
"Give the length of the music, before the measure number `number"
(let loop ((l (or (*timing-sections*)
                  (make-timing-sections first-measure))))
  ; l is in reverse order, last sections to the beginning
  (let* ((section (car l))           ; l is never empty
         (n (vector-ref section 0))) ; a section begins at measure number n
    (if (< number n)                 ; is number in this section ?
      (loop (cdr l))                 ; no ? : prev section. Crash if number < first-measure
      (let ((mom (vector-ref section 1))
            (1measure-len (vector-ref section 2)))
        (mom-add mom (mom-imul 1measure-len (- number n))))))))

% #(define* (measure-number->moment number #:optional first-measure)
% "Give the length of the music, before the measure number `number"
% (if (<= number first-measure)
%   ZERO-MOMENT
%   (let loop ((l (or (*timing-sections*)
%                     (make-timing-sections first-measure))))
%     ; l is in reverse order, last sections to the beginning
%     (let* ((section (car l))           ; l is never empty
%            (n (vector-ref section 0))) ; a section begins at measure number n
%       (if (< number n)                 ; is number in this section ?
%         (loop (cdr l))                 ; no ? : prev section. Crash if number < first-measure
%         (let ((mom (vector-ref section 1))
%               (1measure-len (vector-ref section 2)))
%           (mom-add mom (mom-imul 1measure-len (- number n)))))))))

%%
upToMeasure = #(define-music-function (number) (integer?)
"Return a skip music, which has the same length than the music before measure
`number, according to the timing informations given in \\global."
(if (not (*timing-sections*))                     ; 1st call of upToMeasure ?
  (if (and (defined? 'global)(ly:music? global))
    (*timing-sections* (make-timing-sections 1))  ; done : list initialized
    (ly:error "upToMeasure : no music variable \\global found")))
(let((up-to-moment (if (< number 1) ZERO-MOMENT (measure-number->moment number))))
  (make-music 'SkipEvent
      'tags (list 'from-measure-one ) ; to detect from where it is relative.
      'duration  (make-duration-of-length up-to-moment))))

                      %%% check-length %%%
% The eventual use of \upToMeasure in the `during  parameter of the
% extractMusic function implies a special check.
#(define (check-length from-moment during)
"Returns a valid length of `during, detecting if this music `during is relative
to the first measure or to `from-moment, and checking if the measure
given by the user is not before `from-moment."
(let* ((error-found #f)
       (good-during (music-map
         (lambda (evt)
             (if (memq 'from-measure-one (ly:music-property evt 'tags))
               (let ((len (ly:moment-sub (ly:music-length evt) from-moment)))
                  (if (ly:moment<? len ZERO-MOMENT)
                      (set! error-found #t)
                      (set! evt (make-music 'SkipEvent 'duration
                                        (make-duration-of-length len))))))
             evt)
         (ly:music-deep-copy during))))
 (if error-found #f (ly:music-length good-during))))

%%%%%%%%%%%%%%%%%%%%%%%%%%    extractMusic     %%%%%%%%%%%%%%%%%%%%%%%%%%%%
extractMusic = #(define-music-function (music from during) (ly:music? ly:music? ly:music?)
"Copy all events of `music, if their position relative to the begin of
`music, is higher than the length of `from music and lower that the sum of the
length of `from music and the length of `during music."
 (let* ((from-length (ly:music-length from))
        (during-length (or (check-length from-length during)
                           ZERO-MOMENT))
        (to-length (ly:moment-add during-length from-length)))
    (parameterize ((*current-moment* (ly:make-moment 0/1)))
             (extract-range (expand-q-chords music) from-length to-length))))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% derived functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% used by \extractEnd
#(define (mmR-or-mmS? music) ; Is music = \mmR or \mmS ? (see later)
  (let ((res #f))
    (music-map
       (lambda (evt)    ; \mmR and \mmS have an evt with \tag #'mmWarning
          (if (memq 'mmWarning (ly:music-property evt 'tags))
             (set! res #t))
          evt)
       music)
    res))

extractBegin = #(define-music-function (music during)(ly:music? ly:music?)
 #{ \extractMusic $music s1*0 $during #})
 %%%%%%%%

extractEnd = #(define-music-function (music from)															                             (ly:music? ly:music?)
(let ((during (cond
        ((not (mmR-or-mmS? music)) ;; to extract until the end of the music, set
            mmS)        ;; the during parameter as a quasi-infinite length music
        ((and (defined? 'global)(ly:music? global))
            #{ \tag #'from-measure-one \global #})
        (else                     ;; music is \mmR or \mmS and no \global found
           (ly:music-message music (string-append
              "\n\\mmR and \\mmS are too long to be extracted until their end !"
              "\nPlease, set the length of your music in a \\global variable."))
            #{ s1*0 #}))))       ;; avoid a quasi-infinite length extraction ...
 #{ \extractMusic $music $from $during #}))

 %%%%%%%%

insertMusic = #(define-music-function (music where musicToInsert)(ly:music? ly:music? ly:music?)
 #{
	\extractBegin $music $where
	$musicToInsert
	\extractEnd $music $where
 #})
 %%%%%%%%

replaceMusic = #(define-music-function (music where replacementMusic)(ly:music? ly:music? ly:music?)
#{
	\extractBegin $music $where
        $replacementMusic
	\extractEnd $music { $where $replacementMusic }
#})
 %%%%%%%%

replaceVoltaMusic = #(define-music-function (musicWithVoltas where replacementMusic) (ly:music? ly:music? ly:music?)
(let ((global-struct (map-some-music
        (lambda (evt)                                  ;; skipifies all music ...
           (if (eq? (name-of evt) 'VoltaRepeatedMusic) ;; but keeps volta structure
             (begin
               (ly:music-set-property! evt 'element
                 (skip-of-length (ly:music-property evt 'element)))
               (ly:music-set-property! evt 'elements
                 (map skip-of-length (ly:music-property evt 'elements)))
               evt)
             (and (or (ly:music-property evt 'duration #f)
                      (eq? (name-of evt) 'EventChord))
                  (skip-of-length evt))))
        (ly:music-deep-copy musicWithVoltas))))
   #{
     <<
      \replaceMusic $musicWithVoltas $where $replacementMusic
      $global-struct
     >>
   #}
))

 %%%%%%%%
#(define (extract-during music from during)
"A scheme implementation of extractMusic function, `from and `during as moment."
(parameterize ((*current-moment* ZERO-MOMENT))
  (extract-range (expand-q-chords (ly:music-deep-copy music))
                  from
                  (ly:moment-add from during))))

%%%%%%%%%%%%%%%%%%%%% multiExtractMusic %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define (reduce-seq mus)
"Try to reduce the number of sequential music"
   (define (merge-in-list m l)
     (let ((name (name-of m)))
       (cond
         ((eq? name 'SequentialMusic)
            (let ((elts (ly:music-property m 'elements)))
              (cond ((null? elts) l)  ;; skip m : well not sure it is absoluty safe...
                    ; the \tempo command makes a sequential music of 2 events (the first is a 'TempoChangeEvent). It seems
                    ; that \displayLilyMusic need this sequential music to work ! Is there others Lilypond commands like that ?
                    ((or (eq? 'TempoChangeEvent (name-of (car elts)))
                         (pair? (ly:music-property m 'tags)))  ; a tag can be important !
                       (cons m l))  ; don't reduce the sequential music
                    (else (fold-right merge-in-list l elts))))) ; do reduce
         ((memq name '(RelativeOctaveMusic UnrelativableMusic TransposedMusic))
            (merge-in-list (ly:music-property m 'element) l))
         ((eq? name 'SimultaneousMusic)
             (ly:music-set-property! m 'elements
               (map reduce-seq (ly:music-property m 'elements)))
             (cons m l))
         (else (cons m l)))))

(let ((name (ly:music-property mus 'name)))
  (cond
    ((eq? name 'SequentialMusic)
      (let ((elts (ly:music-property mus 'elements)))
         (ly:music-set-property! mus 'elements
             (fold-right merge-in-list '() elts))))
    ((eq? name 'SimultaneousMusic)
       (ly:music-set-property! mus 'elements
              (map reduce-seq (ly:music-property mus 'elements))))
    ((memq name (list 'RelativeOctaveMusic 'UnrelativableMusic 'TransposedMusic))
       (ly:music-set-property! mus 'element
              (reduce-seq (ly:music-property mus 'element)))))
  mus))

  %% the function make-mEM-func below allows the user to automatically convert
  %% a function to be used in the seq-music parameter of multiExtractMusic.
  %% The new function will have the same name but with the prefix mEM.
  %% See explanations of multiExtractMusic
#(define functionsTagsAlist '())
#(define (make-mEM-func func-symbol)
(if (symbol? func-symbol)
  (let* ((new-symb (symbol-append 'mEM func-symbol))
         (tag-symbol (symbol-append new-symb 'Tag))
         (new-func (define-music-function (param1 param2)
                                                    (ly:music? ly:music?)
                     (make-music 'SequentialMusic 'elements
                       (list param1 param2) 'tags (list tag-symbol)))))
    (ly:parser-define! new-symb new-func)
    (set! functionsTagsAlist
              (cons `(,tag-symbol . ,func-symbol) functionsTagsAlist)))))

%%%%%%%%%%
%% how to use multiExtractMusic : %%%%%%%
%{
\multiExtractMusic from seq-music
Make a series of extractions. `seq-music	must be a sequential music
of the following form :
 {  \musicA  \duringA
    \musicB  \duringB
 	etc...}
The result will be a sequence of this form {resultA resultB etc...} with
 resultA = \extractMusic \musicA \from                     \duringA
 resultB = \extractMusic \musicB {\from \duringA}          \duringB
 resultC = \extractMusic \musicC {\from \duringA \duringB} \duringC
 etc ...
 So the `from parameter of each \extractMusic is automatically computed by
 adding the precedent length.
The last during element can be omitted. If so, the music is extracted up to the
end of the music element.
Setting a during element to a 0 length music, let the \music untouched
(i.e not extract). Ex (in 4/4):
\multiExtractMusic s1*5 { %( extraction will begin measure 6 (5 measures length)
	\vlI s1*2    %% extract 2 measures from measure 6 of vlI part
	R1*3 s1*0    %% add 3 whole-rest
	\vlII s1*9   %% add 9 measures beginning at measure 11 (= 5 + 2 + 3 length)
				  %% from vlII
}

If you have a function with 2 music parameters,  you can create a new one using
make-mEM-func, to be used inside the seq-music parameter of multiExtractMusic.
The new function will have the same name but with the prefix mEM.
The new function will have the same behaviour, but his first parameter will be
first extracted :
\multiExtractMusic s1*0 {
  \func \musicA \paramA     \duringA  % apply func to musicA and paramA
                                      % then extract the result during duringA
  \mEMfunc \musicB \paramB  \duringB  % First extract musicB during duringB
                                      % then apply func to the result and paramB
}

ex :
\include "chordsAndVoices.ly"    %% original name "chord.ly". See :
                                 %% LSR = http://lsr.di.unimi.it/LSR/Item?u=1&id=761
                                 %% LSR = http://lsr.di.unimi.it/LSR/Item?u=1&id=545
\include "changePitch.ly"        %% LSR = http://lsr.di.unimi.it/LSR/Item?id=654
#(for-each make-mEM-func '(cP addNote addVoice addVoiceAlt))
then you can use \mEMcP \mEMaddNote etc...
%} %%%%%

multiExtractMusic = #(define-music-function (from seq-music) (ly:music? ly:music?)
   (define (reduce-seq-elts elts)
   "Try to make a big list with no containers,with only notes"
      (ly:music-property (reduce-seq (make-sequential-music elts))
                         'elements))
(let* ((from-length (check-length ZERO-MOMENT from))
       (elts (ly:music-property seq-music 'elements))
       (len (length elts))
       (n  (+ (quotient len 2) (remainder len 2))) ; len = 9 => n = 9/2 + 1 = 5
       (result-list (make-list n)))
 (if (not (and (pair? result-list)
               (eq? 'SequentialMusic (ly:music-property seq-music 'name))))
   (ly:input-message (*location*)
              "Invalid seq-music parameter! Not a sequential music, or empty.")
   (ly:music-set-property! seq-music 'elements (reduce-seq-elts (map
     (lambda (x)
       (let* ((music (car elts))
              (next-pair (cdr elts))
              (mEM-func? (lambda (m)
                           (let ((tags (ly:music-property m 'tags)))
                              (and (pair? tags)
                                   (assq (car tags) functionsTagsAlist)))))
              (entry->mEM-func (lambda (entry param1 param2)
                                 (let ((func (ly:music-function-extract
                                                 (primitive-eval (cdr entry)))))
                                    (func (*parser*) (*location*) param1 param2)))))
         (if (pair? next-pair)
           (let ((during-length (check-length from-length (car next-pair))))
              (set! music (cond
                ((equal? during-length #f) (make-music 'Music))
                ((equal? during-length ZERO-MOMENT)
                    (if (mmR-or-mmS? music) #{ s1*0 #} music))
                (else
                  (let ((entry (mEM-func? music)))
                    (if entry
                       (let*((es (ly:music-deep-copy
                                     (ly:music-property music 'elements)))
                             (param1 (extract-during (car es)
                                              from-length during-length))
                             (param2 (cadr es)))
                          (entry->mEM-func entry param1 param2))
                       (extract-during music from-length during-length))))))
              (set! elts (cdr next-pair))
              (set! from-length (ly:moment-add from-length
                                               (ly:music-length music))))
           (set! music       ; the last `during parameter has been omitted
               (let ((skip (make-event-chord (list
                              (make-music 'SkipEvent 'duration
                                 (make-duration-of-length from-length)))))
                     (entry (mEM-func? music)))
                 (if entry
                       (let*((es (ly:music-property music 'elements))
                             (elt (car es))
                             (param1 #{ \extractEnd $elt $skip #})
                             (param2 (cadr es)))
                          (entry->mEM-func entry param1 param2))
                       #{ \extractEnd $music $skip #}))))
         music))
     result-list))))
 seq-music))

%%%%%%%%%%%%%%%%%%%%%

multiReplaceMusic =#(define-music-function (music seq-music) (ly:music? ly:music?)
(let ((res music))
  (if (eq? 'SequentialMusic (ly:music-property seq-music 'name))
    (let loop ((pointer-list (ly:music-property seq-music 'elements)))
      (if (pair? pointer-list)
        (let* ((replacement (car pointer-list))
               (next (cdr pointer-list))
               (where (and (pair? next)
                           (car next))))
          (if where (begin
            (set! res #{ \replaceMusic $res $where $replacement #})
               ; ((ly:music-function-extract replaceMusic) ; => \replaceMusic
                ;      (*parser*) (*location*) res where replacement))
            (loop (cdr next))))))))
  (reduce-seq res)))
%%   (reduce-seq res)))
% (make-sequential-music (reduce-seq-elts (list res)))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% shortcuts %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 2 shortcuts wich can be usefull in the `music parameter of extractBegin,
%% when there is a lot of time-signature changes. For ex :
%     \extractBegin \mmR \upToMeasure #25
%  will behave like an automatic multiMeasureRest filler.
% WARNING : to use only with extractMusic functions.
% Just a music like { \compressFullBarRests #infinite-mmR } take 5 mn to compile on my machine
                    %%% mmRest %%%
mmR = { #infinite-mmR \tag #'mmWarning R1 } % The \tag is a way to be recognized
                    %%% mmSkip %%%
mmS = { #(skip-of-length infinite-mmR) \tag #'mmWarning s1 }
 %%%%%%%%


#(define eM extractMusic)
#(define M upToMeasure)
#(define eB extractBegin)
#(define eE extractEnd)
#(define iM insertMusic)
#(define rM replaceMusic)
#(define mEM multiExtractMusic)
#(define mRM multiReplaceMusic)

%{
convert-ly (GNU LilyPond) 2.19.82  convert-ly: Traitement de «  »...
Conversion en cours : 2.19.80
%}
