\version "2.25.6"

%%%%%%%%%%%%%%%%%%%%%% version Y/M/D = 2022/12/28 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LSR = http://lsr.di.unimi.it/LSR/Item?id=773
%% Last modification :
%%   - add same-pitch-as
%%   - optimization of correct-out-of-range

#(define ((set-compare-pitch? . syms) p1 p2)
"syms can be a set of 'above 'below 'equal"
(define (comp? p1 p2)
  (or (ly:pitch<? p1 p2)
      (and (memq 'equal syms)
           (equal? p1 p2))))
(cond
   ((memq 'above syms) (comp? p2 p1))
   ((memq 'below syms) (comp? p1 p2))
   (else #f)))

% Note : a (shift-octave pitch octave-shift) is defined in
% scm/music-functions.scm but not with define-public !
% re-invent the wheel...
#(define (pitch-octavize p limit-pitch . syms)
"Transpose repeateadly pitch p by one octave in the good direction,
while p is above limit-pitch if 'above is in syms, or
while p is below limit-pitch if 'below is in syms.
If in addition, 'equal is in syms, the transposition continue 
also if p equal limit-pitch."
(let* ((compare? (apply set-compare-pitch? syms))
       (octavize (if (memq 'above syms) 1- 1+)))
  (let loop ((new-pitch p))
    (if (compare? new-pitch limit-pitch)
      (loop (ly:make-pitch (octavize (ly:pitch-octave new-pitch))
                           (ly:pitch-notename new-pitch)
                           (ly:pitch-alteration new-pitch)))
      new-pitch))))

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

correctOctave = #(define-music-function (sym limit-pitch music)
                                            (symbol? ly:pitch? ly:music?)
"Corrects octave for notes in `music which are above `limit-note if 'above is
set for `sym, or which are below `limit-note if 'below is set."
(music-map
  (lambda (evt)
    (let ((p (ly:music-property evt 'pitch)))
      (if (ly:pitch? p)
        (ly:music-set-property! evt 'pitch
          (pitch-octavize p limit-pitch sym)))
      evt))
   music))


correctOctaveOutOfRange = #(define-music-function
       (low-pitch high-pitch music)(ly:pitch? ly:pitch? ly:music?)
"Corrects octave of notes not in range `low-note `high-note"
#{
  \correctOctave #'below $low-pitch
      \correctOctave #'above $high-pitch $music
#})

%% general implementation for a custum function (func)
%% func must have one music argument, and return a music.
customOutOfRange = #(define-music-function (low-pitch high-pitch func music)
                      (ly:pitch? ly:pitch? procedure? ly:music?)
"Apply func to notes, out of range `low-note `high-note"
 (if (ly:pitch<? low-pitch high-pitch)
    (music-map
      (lambda (evt)
        (let ((p (ly:music-property evt 'pitch)))
          (if (and (ly:pitch? p)
                   (or (ly:pitch<? p low-pitch)
                       (ly:pitch<? high-pitch p)))
            (func #{ $evt #})
            evt)))
      music)
    music))

%% apply \customOutOfRange to \parenthesize
parenthesizeOutOfRange = #(define-music-function (low-pitch high-pitch music)
                            (ly:pitch? ly:pitch? ly:music?)
"Parenthesize notes out of range `low-note `high-note"
  #{ \customOutOfRange $low-pitch $high-pitch #parenthesize $music #})


colorizeOutOfRange = #(define-music-function (low-pitch high-pitch music)
                         (ly:pitch? ly:pitch? ly:music?)
"Colorize in red, notes out of range `low-note `high-note"
(let ((colorfunc (lambda(evt)
                   (let ((tweaks (ly:music-property evt 'tweaks)))
                     (ly:music-set-property! evt 'tweaks (acons 'color red tweaks))
                     evt))))
  #{ \customOutOfRange $low-pitch $high-pitch #colorfunc $music #}))

% a scheme function : music and range as music.
% range in the form of : <c g'> or { c g' }

#(define (correct-out-of-range music range)
"Same as correctOctave but range is a chord or a sequential music
of 2 notes specifying low and high pitches range."
(let ((ranges (music-pitches range))) ; defined in scm/music-functions.scm
  (if (< (length ranges) 2)
    music
    (correctOctave 'below (first ranges)
      (correctOctave 'above (second ranges)
        (ly:music-deep-copy music))))))

% defines a callback function for arranger.ly users.
% If you don't need any argument in args, use perhaps instead (shorter but not necessarily faster) :
% #(define ((same-pitch-as p1) p2)
%    (equal? p1 p2))
#(define ((same-pitch-as p1 . args) p2)
"Tests octave, notename and alteration equality of pitches p1 and p2.
Ignores octaves if 'any-octave is in args.
For alterations, an other comparison predicate than = can be specified in args. It can be > < >= <= 
To ignore alterations, use (const #t)" ; + - * also works
   (define alteration-comp (or (find procedure? args) =)) ; the alteration predicate
   (define any-octave? (memq 'any-octave args))           ; ignore octave ?
 (and (= (ly:pitch-notename p1) (ly:pitch-notename p2))
      (or any-octave?
          (= (ly:pitch-octave p1) (ly:pitch-octave p2)))
      (alteration-comp (ly:pitch-alteration p1)(ly:pitch-alteration p2))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% tests
%{   %% ajouter un % en début de ligne pour le test -> %%{
music = \relative c' {
  c4 d e f
  g^"g'" a b c
  d e f g^"g''" a b c2
}

\score {<<
  \new Staff \with { instrumentName = "1"}
        \music                               % staff 1
  \new Staff \with { instrumentName = "2"}
        \correctOctave #'above g'' \music    % staff 2
  \new Staff \with { instrumentName = "3"}
        \correctOctave #'below g' \music     % staff 3
  \new Staff \with { instrumentName = "4"}
        \correctOctaveOutOfRange g' g'' \music % staff 4
  \new Staff \with { instrumentName = "5"}
        \colorizeOutOfRange g' g'' \music    % staff 5
  \new Staff \with { instrumentName = "6"}
        \parenthesizeOutOfRange g' g'' \music    % staff 6
>>
}

range = < g' g'' >
\new Staff \with { instrumentName = "4 Alt"}
        $(correct-out-of-range music range)

#(display ((same-pitch-as #{ c #}) #{ cis #}))
#(display ((same-pitch-as #{ c #} (const #t)) #{ cis #}))

%}

%{
convert-ly (GNU LilyPond) 2.25.7  convert-ly: Processing `'...
Applying conversion: 2.25.0, 2.25.1, 2.25.3, 2.25.4, 2.25.5, 2.25.6
%}
