\version "2.20.0"
%% LSR = http://lsr.di.unimi.it/LSR/Item?id=773
%% version Y/M/D = 2020/07/04

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

% a scheme function : music and range as music
% range in the form of : <c g'> or { c g' }
#(define (correct-out-of-range music range)
(let ((low #f)
      (high #f))
(music-map                  ; first pitch -> low, second pitch -> high
  (lambda (evt)
     (let ((p (ly:music-property evt 'pitch)))
        (if (ly:pitch? p) (cond
           ((not low)(set! low p))
           ((not high)(set! high p))))
        evt))
  range)
(music-map
   (lambda (evt)
     (let ((p (ly:music-property evt 'pitch)))
        (if (ly:pitch? p)
          (ly:music-set-property! evt 'pitch
            (pitch-octavize (pitch-octavize p low 'below) high 'above)))
        evt))
    (ly:music-deep-copy music))))

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
#(define musicII (correct-out-of-range music range))

\new Staff \with { instrumentName = "4"}
        \musicII   % staff 6 %}

%{
convert-ly (GNU LilyPond) 2.19.82  convert-ly: Traitement de «  »...
Conversion en cours : 2.19.80
%}
