%% This score is an arrangement for an (amateurs) concert band (= winds + percus + double bass) of :
%%    polovtsian dances of BORODIN
%% The original version can be found here :
%%    http://imslp.org/wiki/Special:ImagefromIndex/22928
%%  Only page 9 to 20 are concerned here.
%%
%% This arrangement acts like a tutorial, for the use of "arranger.ly" functions.
%% To see the final result you will have to follow instructions given
%% step by step.

\include "../arranger.ly"

%%%%%%%% step 0 %%%%%%%%
%% arranger.ly requires first a \global variable with all timing events,
%% then secondly a call of the function "init".

%% 1) \global : here only 1 timing event : 4/4  measure 1
%% Like in lilypond, if no timeSignature is specified, 4/4 is assumed.
%% So global = s1*45 is enough, but \global is also used here to setup
%% other not timing-events

markDef = { % no mark and barnumber at the same time
  \once \override BarNumber.break-visibility = ##(#f #f #t) % only beginning of line visible
  \mark \default
}

setup = {
  \override Score.RehearsalMark.break-visibility = #all-visible
  \set Score.rehearsalMark = #3  % marks begin at C (it is a 2nd mvt)
}

global = {
  \setup
  \tempo "Allegro vivo"
  \key f \major
  s1*18
  \markDef %% C
  s1*16
  \markDef %% D
  s1*7
  \tempo "Accelerando"
  s1*4
  \bar "|."
}

% define C and D as 19 and 35
#(def-letters '(19 35) 2) % 2 = skip 2 letters (A and B)

%% 2) init (initialization of all instruments)
%% As arranger.ly functions allow list of instruments as parameter, it is worthwhile
%% to take a few minutes and declare a few sub-lists of instruments
%% (Note that names here are shortcut for french instrument names)

fls = #'(fl1 fl2)            % flutes
flAll = #(cons 'piccolo fls)

cls = #'(cl1 cl2 cl3)       % clarinets
% cl12 = #'(cl1 cl2)        ; not allowed in Lilypond => use scheme déclaration
#(define cl12 '(cl1 cl2))
#(define cl23 '(cl2 cl3))

saxs = #'(alt1 alt2 saxT saxB) % saxophones
altTen = #'(alt1 alt2 saxT)
alts = #'(alt1 alt2)

tps = #'(tp1 tp2 tp3)     % trumpets
#(define tp12 '(tp1 tp2))
#(define tp23 '(tp2 tp3))

cors = #'(cor1 cor2)      % horns
tbs = #'(tb1 tb2)         % trombones
tubas = #'(euph tuba)     % euphonium (euph) + tuba

percus = #'(xylo percu timbales)   % percu = small percu (tambourin + triangle)

%% Here is the full list of instruments (named "all").
%% To build it, we use the arranger.ly function : lst. It returns
%% a flat list from the above list of symbols, and from these following alone symbols :
%% 'htb (oboe, "hautbois" in french), 'bsn (bassoon/fagot), 'clB (bass clarinet)
%% and 'ctb (double bass ("contrebasse" in french)).

all = #(lst flAll 'htb 'bsn cls 'clB saxs tps cors tbs tubas 'ctb percus)

% call the init function
#(init all) %% All instruments are now initialized by a multiRest R1*45 (= length
% of \global). We can now use arranger.ly functions.

% => Please go to NOTES.ily to continue.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2 functions for score (SCORE.ly) and parts.

%% 1 instru in a Staff
#(define* (instru->music instru #:optional (clef "treble"))
(sim                   ; << ... >> (simultaneous)
  (make-clef-set clef) ; = \clef
  global
  (obj->music instru)  ; music of instru
  (add-dyn instru)))   % dynamics of instru

%% 2 instrus in the same Staff
#(define* (split-instru instru1 instru2 #:optional (clef "treble"))
(split                       ; << ... \\ ... >>
  (sim                       ; instru1 : upper voice
    (make-clef-set clef)
    global
    dynamicUp                ; dynamics above Staff
    mergeDifferentlyHeadedOn
    mergeDifferentlyDottedOn
    (add-dyn (list 'xor instru1 instru2)) ; instru1 dyn if instru2 has not the same
    (obj->music instru1))   ; instru1 music
  (sim                      ; instru2 : lower voice
    (add-dyn instru2)       ; instru2 dynamics
    (obj->music instru2)))) % instru2 music

%% func to draw instrumentName vertically, in scores.
%% ATTENTION !!! Not compatible with accents ; ê û etc ...
#(define-markup-command (char-column layout props name baseline) (string? number?)
"Print each character of string `name in a centered-column"
  (interpret-markup layout props
   (if (<= (string-length name) 2)
     (markup name)
     (markup (make-override-markup (cons 'baseline-skip baseline)
		      (make-center-column-markup (map
                (lambda(c)
                  (make-line-markup (list
                    (make-hspace-markup 0.3)
                    (make-simple-markup (string c))
                    (make-hspace-markup 0.3))))
                (string->list name))))))))

%% each ly parts must define part in the first line : ex : #(define part 'fl1)
#(define (part? arg)
   (and (defined? 'part)
        (if (list? arg)
          (memq part arg)
          (eq? part arg))))

#(define-markup-command (timbalesTuning layout props chord-str size) (string? number?)
"Make a mini score markup of a chord, to indicate the appropriate tuning of the timbales.
chord-str is a chord enclosed by 2 quotation marks"
    (interpret-markup layout props
#{ \markup \score {
  \new Staff \with {
        \remove "Time_signature_engraver"
        fontSize = #size
        \override StaffSymbol.staff-space = #(magstep size)
        \override TextScript.self-alignment-X = #CENTER
        \override TextScript.font-shape = #'italic
        }
    {
      \once \override Score.Clef.stencil = ##f
      \clef bass
      \cueClef bass
      \override TextScript.font-size = #(- textSize size) % textSize is defined below
      \cP c1*1/8 $(eval-string (string-append "#{ " chord-str " #}"))
    }
  \layout {
    ragged-right = ##t
    indent = 0\cm
  }
} #}))
 
 
%%%% scheme->lilypond-names
% 'cl1 'cl2 'cl3 will be converted in 'clI 'clII 'clIII
% Caution : digits must be only at the end of the name : 'ab12cd returns an error
#(define (scheme->lilypond-names sym)
"Digits at the end of a symbol name are converted in a roman numeral.
Returns the symbol converted"
(let* ((s (symbol->string sym))
       (i (string-rindex s char-set:digit)))
  ;; (format #t "~a : ~a\n" s i)
  (if i (string->symbol 
           (string-append (string-take s i)
                          (format #f "~@r" (string->number (string-drop s i)))))
         sym)))


%%%%%%%%%%%%%%%%%%%%% layout settings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define textSize -2)
#(define dynamicSize -2)
#(if (not (defined? 'header-sep))(define header-sep 4)) % can be tweaked in separate parts

myLayoutSet = \with  {
  \override TextScript.font-size = #textSize
  \override TextScript.font-shape = #'italic
  \override TextScript.whiteout = ##t
  \override TextScript.outside-staff-priority = #1
  \override DynamicText.font-size = #dynamicSize
  \override DynamicText.font-series = #'medium %% medium bold, bold-narrow
  \override DynamicText.whiteout = ##t
  \override DynamicText.self-alignment-X = #RIGHT
  \override DynamicLineSpanner.font-size = #dynamicSize
  \override DynamicTextSpanner.font-size = #dynamicSize
  \override TextSpanner.font-size = #textSize
  \override TextSpanner.font-shape = #'italic
  \override Script.font-size = #(1- textSize)
  \override TrillSpanner.font-size = #(1- textSize)
  \override Hairpin.height = #0.4
  % \override Hairpin.minimum-length = #5.5
  %\override Hairpin.whiteout = ##t
}

myStaffLayoutSet = \with {
  printKeyCancellation = ##f % will print anyway 2 naturals from key d \major to c
  extraNatural = ##f
  \override AccidentalCautionary.parenthesized  = ##f
  autoAccidentals =  #`(Staff ,(make-accidental-rule 'same-octave 0))
  autoCautionaries = #`(Staff ,(make-accidental-rule 'any-octave 1))
}

globalLayout = \layout {
  \context { \Score
    skipBars = ##t
    \override MultiMeasureRest.expand-limit = #1
    \override MultiMeasureRest.minimum-length = #15
    \override MultiMeasureRestNumber.font-size = #(1- textSize)
    \override BarNumber.font-size = #(1- textSize)
    \override BarNumber.self-alignment-X = #CENTER
    \override RehearsalMark.font-size = #textSize
    \override RehearsalMark.break-visibility = ##(#f #t #t)
    \override MetronomeMark.font-size = #textSize
    markFormatter = #format-mark-box-letters
    %	\remove Bar_number_engraver
  }
  \context { \Staff \myStaffLayoutSet }
  \context { \Voice \myLayoutSet }
  \context { \DrumVoice \myLayoutSet }
  \context { \Dynamics \myLayoutSet }
}

\layout { \globalLayout }



#(define-markup-command (bookTitle layout props) ()
"Standard bookTitleMarkup with instrument markup modified"
(interpret-markup layout props
#{
  \markup {
    \override #`(baseline-skip . ,header-sep)
    \column {
      \line \vcenter { \italic \fromproperty #'header:instrument
                       \hspace #1
                       \fromproperty #'header:instrumentMore }
      \bold \larger \larger \fill-line { "" \fromproperty #'header:title  "" }
      \fill-line \vcenter { "" "" \vcenter\fromproperty #'header:composer }
      }
    }
#}))

\paper {
  bookTitleMarkup = \markup \bookTitle
}

\header { title = "(2nd mvt)" }
