\version "2.19.80"
#(ly:set-option 'relative-includes #t)

\include "init.ily"

% step 0
% If you compile now SCORE.ly, you will have a score filled with multiRests.
% It is the result of the "init" function called in init.ily file.

% As arranger.ly allows a work flow "by ideas", we are going to deal first
% with themes (2 themes), then with basses and syncopated music.
% Please, compile SCORE.ly after each steps. You just have to comment the %{
% with a second percent % character, after lines beginning with % step xx
%%  =>  %{  will become  %%{


% step 1 - Please comment line below
%{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% themeI %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% themeI is theme in 16th notes, playing first by clar solo in the original score.
%% We use pattern because rhythms and articulations are repeated several times.
patCommon = { c8-. c->~ c16( c c c) c16( c c c) c16( c c c) | }
patI = { c8-. c->~ c16(  c c8) c16( c c c) c16( c c c) |
         \patCommon }
patII = { \patCommon
          c8.->( c16 c4)~ c8 c->( c c) | }
themeI = \transpose d c \relative c' { % clar in B => themeI in concert pitch
  \cPI { % shortcut for \changePitch \patI { notes ...}
    a'' a b c c d c b b c b a   % mes 3
    g g a g fis fis g fis f e f fis g
    a a b c c d c b b c b a
    g g a g f ees f ees d c d ees g
  }
  \cPII { % % shortcut for \changePitch \PatII  { notes ...}
    c, c d c bes aes bes aes g f g aes c % mes 7
    g bes g a bes d
    aes aes bes aes g f g f ees d ees f aes
    g bes g a bes d
  }
}

#(begin  ; scheme bloc
; the base function of arranger.ly is rm : rm = replace music
; 2 variants of this function can be used : x-rm et rm-with
; x-rm to add the same music to several positions
; rm-with to add musics to a same set of instruments
(rm 'cl1 3 themeI)                    ; adds themeI to cl1 at measure 3
(rm 'cl1 3 (txt "Solo" UP RIGHT))     ; adds text, direction UP, self-alignment-X = RIGHT
;; m.11 : in original score, themeI measure 11, is played one octave higher by flutes.
        ; The theme is also divided between piccolo and the solo flute.
(rm 'piccolo 11 (octave +1 themeI) 15) ; takes only from 11 to 15
  (rm 'piccolo 11 (txt "Solo" UP RIGHT))
(rm 'fl1 12 (octave +1 themeI) 11) ; does *not* take from 11 to 12 (11 < 12).
(let ((rests #{ r8 r4 r2 | #})) ; define temp variables
  (x-rm 'piccolo rests '(12 8) '(14 8)) ; adds rests a 8th after first beat of m.12 and 14
  (x-rm 'fl1 rests '(13 8)))            ; (rm would have worked here)
(rm-with 'fl1 12 (txt "Solo" UP 0.5)    ; add texts to fl1 : m.12 and 15
              15 (txt "Tutti" UP -0.15))
;; m.19 = C
 (rm cls C themeI)                 ; add themeI to clarinets 1 2 3
 (rm altTen C (octave -1 themeI))  ; idem for altos and tenors, but 1 octave lower
 (octavize +1 alts '(25 4 16) '(26 -16)); m.25 some altos notes are very low. Correct them.
   ;; '(25 4 16) = after m.25 + a 4th + a 16th
   ;; '(26 -16) = a 16th before m.16
;; m.27
 (rm '(cl1 htb) 27 themeI) ; themeI to cl1 and oboe m.27
   (octavize +1 'htb 31 D)
 (rm flAll 27 (octave +1 themeI)) ; piccolo, flutes 1 et 2
 (octavize -1 fls 27 '(28 8) / 29 '(30 8)) ; as in original score. The / is an optional delimiter
 (rm 'piccolo '(33 2) #{ ees''8 r r4 | #})
 (rm 'xylo 27 ((set-del-events 'SlurEvent) themeI)) ; delete slurs of themeI
)

%} % end step 1

% step 2 - Please comment line below :
%{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% themeII %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% In original score, themeII is played m.11 page 10, by english horn, basson, clarinettes
patI = { c16->( c c8)~ }
themeII = \relative c' {
  bes8->( c d4) \tuplet 3/2 { e8( d c} bes4) |
  d8( c16 bes a2) f'4 |
  \cPI { g, a bes } bes2 e,4 |
  aes2-> f |
  ges'2-> bes,-> |
  \cPI { aes bes c } c2. |
  ees2-> ges,-> |
  \cPI { aes g f } f2. |   % { aes g! f }
}

#(begin
;; m.11
(rm (lst cls tps 'bsn) 11 themeII) ; themeII to clarinets, trumpets, basson
(rm tps '(14 -4) #{ \parenthesize e4 #}) ; the e (fis in b) is low !
(rm 'cl1 11 (txt "uni" UP))
;; m.19 = C
; octave has an extended syntax :
;   (octave 1 2 .. music) => (list (octave 1 music) (octave 2 music)..)
(rm (list 'piccolo (lst fls 'htb) tps) 19 (octave +3 +2 +1 themeII))
;; tp2, tp3 corrections
 (rm-with tp23 '(21 -4) #{ r4 #} / 23 #{ r2 #})
 ; oboe correction
 (octavize -1 'htb '(19 2) 21 / 23 '(25 2)) ; as original score
 (rm 'htb '(26 2) #{ r2 #})
 ; m.26 : bridge themeII/themeI flutes :
;(copy-to flAll (octave +1 cl1) '(26 4) 27) ; OK but slurs warnings
(rm (list 'piccolo fls) '(26 4) (octave 3 2 #{ f4~ f8 g->( aes c') #})) ; by hand
;; m.27 : themeII to brass
(rm (lst cors tbs 'euph) 27 themeII)
; In the original score, slurs and accents of themeII are changing between
; m.27 to 30 ! We remove current articulations and add some new using :
;         apply-to, set-del-events, set-arti and compose
(let ((new-arti (set-arti #{ c-> c c c-> c c c % 27
                             c-> c( c) c c     % 28
                             c-> c c c c #}))
      (del-arti (set-del-events 'ArticulationEvent 'SlurEvent)))
  (apply-to (lst cors tbs 'euph) (compose new-arti del-arti) 27 30)
 ))

% step 3 - Please comment line below :
%{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% basses %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% In original score, basses are build with 1 note repeated a lot of times
noteI = f,2-!    % m.1 -> 19
noteII = c,2->   % m.19 -> 27
noteIII = c,1	% m.19 -> 27
noteIV = f,2		% m.27 -> end
#(begin
;; The "fill" function computes automatically the number of repeats required
;; to fill a section with a pattern
(fill 'clB noteI 1 19) ; repeats noteI (36 times ?)
(fill '(saxB ctb) noteI 11 19)
(fill '(ctb saxB) noteII 19 27)
(fill 'tuba noteIII 19 27)
(fill '(tuba ctb clB) noteIV 27 'end)
  (octavize -1 'ctb 42 'end)
(x-rm 'saxB #{ f,1-> #} D 37 39)
)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% syncopation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% In original score, syncopations begins m.1 in viola part. As each chord
%% from m.1 to 10 is repeated several times, we used in patterns, the function
%% \samePitch. It keeps the chord unchanged during all the pattern
% patI = \samePitch { r8 c4-> c8 r8 c4-> c8 | }       % original articulations
patI = \samePitch { r8 c4---> c8-. r8 c4---> c8-. | } % sligthly changed for winds
syncop = \cPI { % \changePitch \patI ...
  <c g> q q
  <d a> % mes4
  <c g>
  <des aes>
  <ges des'> %mes 7
  <f c'>
  <ees bes>
  <f c'> % mes 10
}
% The dispatch-chords function is the way to dispatch notes into monodic instruments
% In m.1, we want to assign the syncopations to clarinets but same viola notes are
% too low for a clarinet. So we reverse chords of the viola, one time upward from
% m.1 to 7.
% For reversing chords, there is 2 functions : reverse-chords and set-reverse
% They do the same things but with a different syntax. In arranger.ly, functions
% beginning by set-[...] have a syntax compatible with apply-to

#(let* ((rev (set-reverse 1))                    ; rev will reverse chords 1 time upward
        (syncop-alt (apply-to syncop rev 1 7)))  ; apply rev to the section 1-7
(dispatch-chords cl23 1 syncop-alt) ; note 2 of chords to cl2, note 1 to cl3
(rm 'cl1 1 syncop-alt 3) ; cl1 gets whole chords. (divided by stand).
  (rm 'cl1 1 (txt "div." UP))
(add-voice2 'cl1 3 syncop-alt 1) ; In m.3, voice1 = clar solo, voice2 = cl1
;; m.11
;; sax tenor range allow the low c. We can use syncop instead of syncop-alt
(dispatch-chords altTen 11 syncop 9) ; 9 to delete the 2 intro measures
;; m.19 = C
(dispatch-chords cors 19 syncop 17)
(dispatch-chords '(bsn clB) 19 syncop 17)
(rm 'euph 19 syncop 17)
;; m.27
(dispatch-chords altTen 27 syncop 25)
(let((syncop-m27 (cp1 (rel  ;; scheme version of \cPI \relative
      #{ <g' g'> <a d> <g g'> <aes des>
         <des, ges> <f, c'> <bes ees> <f c'> #}))))
  (dispatch-chords cl23 27 syncop-m27)
  (rm 'tp1 27 syncop-m27))
(copy-to 'tp1 cl1 32 33 / 34 35) ; copy sections from cl1 to tp1
(x-rm 'tp1 (txt "div" UP) 27 33)
(x-rm 'tp1 (txt "uni" UP) 32 34)
(fill tp23 #{ c''1-> #} 27 35) ;
;; D = m.35 : 2 sections 35-42 and 42-end, with repeated cells.
;; em = extract music : to retrieve a previous cell
;; fill-with to repeat the cell to fit a given section
(let ((syncop-m35 (fill-with (em syncop 3 5) 35 42)) ; 35-42 fill with syncop cell 3-5
      (syncop-m42 (fill-with (cp1 #{ <f c'> q q #} ) 42 'end)) ; cp1 = \cPI
      (rev-1 (set-reverse -1)))  ; reverse chords downward
 ; ; seq = sequential : { ... }
 (dispatch-chords '(tb1 tb2) 35 (seq syncop-m35 syncop-m42))
 (rm 'euph 35 (seq (txt "div.")
                   syncop-m35
                   (rev-1 syncop-m42))) ; reversed chords
 ;; function "note" extract n-th note of a chord
 (rm 'bsn 35 (seq (note 1 syncop-m35)
                  (octave -1 (note 2 syncop-m42))))
))


% step 4 - Please comment line below :
%{

%%%%%%%%%%%%%%%%%%%%%%%%%%% D (m.35) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% D is made with fragments of themes I and II.

% frags of themeI : m.36, m38 (a 3rd lower), m.40 (a 6th lower)
#(let ((m (em themeI 4 5 3))          ; themeI begins m.3 (not 1)
       (3rd- (set-transp 0 -2 0))     ; a 3rd minor downward (c' -> a)
       (3rd-- (set-transp -1 -2 0))   ; a 3rd minor and an octave (c' -> a,)
       ;(6th- (set-transp 0 -5 0))    ; no because not always 6th minor
       (6th- (to-set-func (lambda(x)  ; called by the callback of a music-map func
               (let((p (ly:music-property x 'pitch #f)))
                 (if p (ly:music-set-property! x 'pitch
                         (ly:pitch-transpose p
                           (if (member p (list #{ g'' #} #{ d'' #}))
                             #{ es #}         ; 6th Maj for g'' and d'' (c' -> es)
                             #{ e #}))))))))) ; 6th min for others (c'-> e)
    (rm-with 'cl1 36 m / 38 (3rd- m) / 40 (6th- m))
    (rm '(tp1 cl2 saxT) 36 (octave -1  m))
    (rm-with 'alt1 38 (3rd-- m) / 40 (seq (txt "1 seul" UP RIGHT) (6th- m)))
    (rm fls 40 (6th- m))
    (rm 'cl1 42 (octave -1 m)) ; the end is wrong : we correct it later
    )

% frags of themeII : m.35 37 39 41 : an accent is added in the original (3rd beat)
#(let ((patI (ca #{ c c c c-> c c c #} (em themeII 11 12 11))) ; ca = change-arti shortcut
       (notes35 #{ bes c d e d c bes #})
       (notes37 #{ e f g a g f e #})
       (notes39 #{ bes c d e d c bes #}) ; = notes35
       (notes41 #{ g a bes c bes a g #}))
   (rm (list fls alts) 35 (cp1 (rel 2 1 notes35))) ; extented syntax of rel (\relative)
   (rm (list (lst fls 'htb)(lst 'cl3 tp23)) 37 (cp1 (rel 1 0 notes37)))
   (rm 'tp1 39 (seq (txt "solo")(cp1 (rel 1 notes39))))
   (rm 'cor1 41 (seq (txt "solo" UP)(cp1 (rel 1 notes41)))))

% held notes
#(begin
(dispatch-chords ; 5 notes in chord, 5 elts list.
  `(htb ,tp23 tp1 (cor1 cl2) (cor2 cl3)) D #{ <c' g' e'! bes' g''>1-> #})
;          ↳ = (tp2 tp3)
(rm '(cl1 saxT) D (list #{ <e'! bes'>1-> #} #{ <e'! g'>1-> #}))
(dispatch-chords (zip alts cors) 37 #{ <bes e'>1 #})
;                  ↳ = ((alt1 cor1)(alt2 cor2))
(dispatch-chords (zip cl23 tp23) 39 #{ <c' e'>1 #})
(add-voice2 'tp1 39 #{ bes'1 #})
(rm 'cor2  41 #{ e'1 #})
(dispatch-chords cors 42 #{ <a f'>1~ q~ q~ q2 r #})
(rm 'cor1 42 (txt "tutti (sourdine)" UP))
)

% repeated notes
#(let ((patI #{ \samePitch { c8-. c-> ~ c2.} #})
       (chord36 #{ <f a> #})
       (chord38 #{ <a f'> #})
       (chord40 #{ <d 'f'> #}))
(dispatch-chords (zip '(htb) fls) 36 (octave 2 (cp1 chord36)))
(dispatch-chords (zip cors alts `(,tp23 cl3)) 36 (octave 1 (cp1 chord36)))

(dispatch-chords `(,fls htb) 38 (octave +1 (cp1 chord38)))
(dispatch-chords (zip cl23 '(alt2 saxT) cors) 38 (cp1 chord38))

(dispatch-chords (zip '(htb) cl23) 40 (cp1 chord40))
(rm 'alt2 40 (cp1 chord40))
(if (not (part? 'score)) ; to not overload the score
  (add-voice2 'alt1 40 (seq (txt "div." DOWN 2.1) (cp1 chord40))))
)
%}

% step 5 - Please comment line below :
%{

%% m.42 (accelerando bis end)
% The following code is an example of use of function tweak-notes-seq.
% (tweak-notes-seq '(1 2 3 2 1) #{ c d e | f g a #})
%   => #{ c d e d c
%         f g a g f #}
% set-tweak-notes-seq is the other form of tweak-notes-seq.
% The same notes (grouped by 5), are used to build 2 sequences of notes,
% which are then used with 2 patterns.

% The descent of 16h notes (violin 1 and 2 in original score) :
#(define descent1 (set-tweak-notes-seq '(2 1 2 3 3 2 3 4 5)))
patI = { c16( c c c) c( c c c) c8 r r4 } % associated pattern
% We want to double first note every four 16th with a 8th note.
#(define descent2 (set-tweak-notes-seq '(2 3 5))) % notes 1 4 ignored
patII = { c8 r c r c r r4 }

#(let ((m1 (rel #{  % the base sequence of 5 notes
         f e d des c | e' d cis c c | bes' a g f f #})) ; violin 1
       (m2 (rel 1 #{
         bes a g f f | g' f e d c | e' d cis c c #})))  ; violin 2
   ;; 16th
   (rm '(cl1 fl2) '(42 2) (cp1 (descent1 m1)))  ; violin 1
     (rm 'fl2 42 #{ R1 r2 #})         ; very low for a flute
     (rm '(cl1 fl2) '(45 2) #{ r2 #}) ; changed afterwards, by final note.
   (rm (lst alts 'fl1) 43 (cp1 (descent1 m2)))  ; violin 2
   ;; 8th
   (let ((m (cp2 (descent2 m1))) ; doubles violin 1 en 8th. start-pos : '(42 2)
         (r2 #{ r2 #}))
     ; the optional obj-start-pos arg of function em should be '(42 2). But as :
     ;   (em m n p 42)  (n et p as integer)
     ; returns the same music as :
     ;   (em m '(n 2) '(p 2) '(42 2))
     ; we use this to lighten the code
     (rm '(cl3 tp3) 42 (seq r2 (em m 42 43 42) r2)) ; (em m '(42 2) '(43 2) '(42 2))
     (rm 'tp1 43 (seq r2 (em m 43 44 42) r2))
     (rm 'htb 44 (seq r2 (em m 44 45 42) r2)))
   (let ((m (cp2 (descent2 m2)))) ; doubles violin 2 in 8th. start-pos : 43
     (rm '(cl2 tp2) 43 (em m 43 44 43))
     (add-voice1 'tp1 44 (em m 44 45 43))
     (rm 'piccolo 45 (octave +1 (em m 45 46 43))))
   (let ((mute "sourdine")) ; = mute (in french).
     (rm cors 41 (txt mute))
     (rm 'tp3 '(42 2) (txt mute (if (part? 'score) DOWN UP)))
     (rm 'tp2 43 (txt mute UP))
     (rm 'tp1 '(43 2) (seq (txt "div." UP)(txt mute UP)))
     )
   (rm 'cl3 '(43 2) #{ f2~ | 1~ | 2 #})
   (rm 'cl2 44 #{ r2 c''2~ | 2 #})
   (rm 'saxT 44 #{ c'1~ | 2 #})
   (dispatch-chords alts 45 #{ <a' f''>2 #})
   (rm (lst cors altTen cl23) '(45 2) #{ r4 r\fermata #})
   (rm tps 45 #{ r2 #})
   ; xylo => all 8th notes (m1 and m2) in a << >> structure
   ; As Lily complains when a << note rest >> collision occurs, a new
   ; pattern pat3 is needed : r4 in patII becomes s4 in pat3.
; #!
   (let ((pat3 #{ c8 r c r c r s4 #}))
     (rm 'xylo 42 (sim
       (seq #{ r2 #} (cp pat3 (descent2 m1)))
       (seq #{ s1 #} (cp pat3 (descent2 m2))))))
; !#
;; In the following code, we want to add a glissando to follow voices in xylo part.
;; Just replacing the above pat3 by a new pat3 below works well, but some glissandi
;; don't go to the right voice. Some adjustments are needed.
;; You can comment the 4 code lines above by removing the ; before #! and !#, and
;; uncomment the code below by adding a ; before the #! and !#.
;; As we want that musics below start at measure 42, it requires that copy-to uses
;; the optionnal parameter #:source-start-pos. We prefer here to use the arranger
;; function : (at 42 music) ; It will insert musics at m.42 in a skip s1*45
#!
     (let* ((pat3 #{ c8 r
                     c \glissando \override NoteColumn.glissando-skip = ##t
                     r \revert NoteColumn.glissando-skip
                     c r s4 #})
            (musics (list (seq #{ r2 #} (cp pat3 (descent2 m1)))
                          (seq #{ s1 #} (cp pat3 (descent2 m2)))))
            ; (apply func args => (func arg1 arg2 ...)
            (music12 (at 42 (apply sim musics)))            ; music12 start at m.1
            (music21 (at 42 (apply sim (reverse musics))))) ; reverse voices
       ; we split xylo part in sections,
       (copy-to 'xylo
         music12  42  '(43 4) / '(44 -4)   '(44 4) / '(45 -4) 'end /
         music21      '(43 4)   '(44 -4) / '(44 4)   '(45 -4)))
!#
   ; m.45 3rd beat : final chord
   (dispatch-chords '(piccolo (xylo fl1) (fl2 htb) tp1 tp2 tp3 cl1 tb1
                       (tb2 euph) bsn (tuba ctb clB))
     '(45 2)
      #{ <f, c f c' f' a' c'' f'' a'' c''' f''''>8-! r r4\fermata #})
   (add-notes (lst fls 'xylo 'cl1 'euph) '(45 2)
     (chords->nmusics 5 #{ <c a a'' c''' f'''> #}))
     ; => (list #{ f'''8 #} #{ c'''8 #} #{ a''8 #} #{ a8 #} #{ c8 #}))
) % end step 5


% step 6 - Please comment line below :
%{
%%%%%%%%%%%%%%%%%%%%%%%%% miscellaneous %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% As bass voice are a little boring and tiring for a wind instrument,
%%  we try to animate them a bit...
patI = { \repeat unfold 2 { c8-! \samePitch {c4 c8}} | }
newBasse = \cPI { % m.27
  f, c | f, d | f, c | f, des | f, ges | f, f | f, ees | f, f
} % m.35 = D

#(begin
(rm '(bsn clB saxB) 27 newBasse)
(copy-to 'saxB tuba 42 'end)
;; m30 : perhaps renforcing cl1 is good => add themeI to alt1
(rm 'alt1 '(30 2) themeI 27) ; themeI begins at 27

;; themeI demands a high level of virstuosity from the players. We create below an
;; ossia, accessible at the top level scope. It can be combined with any instrument
;; playing themeI. We add here the ossia to xylo.
(def! 'ossia) ; declare 'ossia to parser. ossia will be a skip of the same length as global
(rm 'ossia 27 (cp                       ; change-pitch
  #{ s2 c8 r c r | s4 s8 c c r c r #}   ; pattern
  (rel 2 #{ \insert { \mergeDifferentlyHeadedOn s2 } % ossia will be a new Voice
            bes a | f e d
            bes' a | f des bes #})))
(def! 'ossia (em ossia 27 31)) ; we define 'ossia again but cut unused music
(add-voice1 'xylo 27 ossia)
(rm-with 'xylo 27 (txt "xylo" UP 1.5) /
             '(27 2) (txt (markup "ossia" (#:general-align Y 0 "↘")) UP RIGHT 5) /
               31 (txt "(idem ossia ad lib)" UP LEFT))
(if (part? 'score)
  (begin
    (x-rm 'cor1 (make-clef-set "bass") C)
    (x-rm 'cor1 (make-clef-set "treble") D)
  ))
(rm (if (part? 'score) 'tp2 tp23) 27 (txt "(en dehors)" UP))
)

% step 7 - Please comment line below :
%{

%%%%%%%%%%%%%%%%%%%%%%%%%%% percus %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% percu and timbales are treated separately.
%  As we plan to make a percu-score, we "tag" it as a score.
#(define score? (part? '(score percu-score))) % (part? is defined in "init.ily")

timbI = { f,4-! r f,4-! r } %
timbII = { c4-! r c4-! r }
timbIII = { f,4-> r f,4-> r } % articulations change
tambourinI = \repeat unfold 2 {r8 \stemDown c4^"o" \stemUp c8}
tambourinII = { c8:32 c c[ c] c:32 c c[ c] | }
triangleI = e1:32
triangleII = { e8 r r4\fermata }
#(begin
((if score? fill fill-percent) ; For separate part, timbales is filled with % repeats
 'timbales timbI 1 C / timbII C 27 / timbIII 27 D)
(if (not score?) (rm 'timbales 1 #{ \set countPercentRepeats = ##t #}))
;(rm 'timbales D (txt "Muta in ré, la")) ; = "change to D, A"
(if (not score?)   ; only for timbales or percu-score
  (rm 'timbales D (txt
    (markup #:timbalesTuning "<a, d>_\"(accord)\"" -4) UP))) ; tuning

(fill 'percu tambourinI C 27 / tambourinII 27 D
             triangleI 42 46 / triangleII '(45 2) 46)
(rm-with 'percu C (txt "tambourin" UP
                     (case part
                       ((percu percu-score) LEFT)
                       ((score) 1.5)))
                    ;(if (part? 'percu-score) 1 1.5)) /
                42 (txt "triangle" UP (if (part? 'percu) LEFT 0.4)))
                     ;; 0.4));(if (part? 'percu-score) 0 0.5)))
(rm 'percu 27 #{ \stemNeutral #})

; re-define 'percu to be in \drummode
(def! 'percu (pitches->percu percu 'tomml ; def instru (central position b' as in original score)
                           #{ e #} 'hihat ; for triangle : comment to keep original 'tomml
                           ))
)
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%% Dynamics %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Requirements for dynamics : %%%%%
% - If several instruments have the same set of dynamics in a given section, we don't want
% to add each dynamics in each instrument separately.
% - An instrument don't have necessarily the same quantity of dynamics in a separate
% part and in a score. (scores often needs to be lightened to be more readable).
% - If 2 instruments are on the same staff (in the score for ex), dynamics must not be doubled.
% - Dynamics positions must be accurate and independent from the notes positions. It requires
% generally a new voice with appropriate skips
% - Dynamics positions have to be easily adjustable (tweakable).
%
% The function assoc-pos-dyn return an associated list. The syntax has the following form :
% (assoc-pos-dyn "1 p / 4 < / 8 f" 'instru1
%                "10 dim / (13 8) ^mp / 14 _p" '(instru1 instru2 instru3) ....)
% A string dynamic like "8 f#1#-2" will produce the following music :
%      <>-\tweak self-alignment-X 1 -\tweak extra-offset #'(0 . -2) -\f
% If the name of the returned associated list is assocDynList, add-dyn can be used :
%    (add-dyn 'instru1) is equivalent to the music :
%    { <>\p s1*3 <>\<  s1*4 <>\f
%      s1*2 <>\dim s1*3 s8 <>\mp s8 s4 s2 <>\p ... }
% For dynamics of several instruments, 3 operators can be used : and or xor
%    (add-dyn '(xor instru1 instru2)) => only "1 p / 4 < / 8 f" is kept

assocDynList = #(assoc-pos-dyn)  % No dynamics for now (has to be here for steps 1 to 7))

% step 8 - Please comment line below :
%{

% 2 new dynamics are needed
crescPocoAPoco = #(make-music 'CrescendoEvent
                              'span-direction START
                              'span-type 'text
                              'span-text "cresc. poco a poco")
dimtext = #(make-dynamic-script #{ \markup \normal-text "dim." #})

%% We use here guile format function. As parameters are a bit "cryptic",
%% here is a remainder :
%% (see  https://www.gnu.org/software/guile/manual/guile.html#Formatted-Output  )
%% Syntax : (format dest fmt arg0 arg1 arg2 ..)
%% in fmt :
%%   ~a : convert to string arg0 then in next call, arg1 then arg2 etc
%%   ~0@*~a : convert to string always arg0
%%   ~1@*~a : convert to string always arg1; idem for ~2@*~a  ~3@*~a etc ...
%%   ~@*~a (without number before @) is equivalent to ~0@*~a

#(define ((set-dyn fmt) arg0 . args)
   (apply format #f fmt arg0 args))
#(define theme1-dyn ; dynamics of head of theme1 after D
   (set-dyn "(~a 4 16) < / (~@*~a 4 8 16) ! / (~@*~a 2 8) > / (~@*~a 2 4 8) !" ))
       % => (pos 4 16)       (pos 4 8 16)       (pos 2 8)       (pos 2 4 8)
#(define theme2-dyn ; dynamics of head of theme2 after D
   (set-dyn "(~a 8) < / (~@*~a 4) ! / (~@*~a 2) > / (~@*~a 2 4) !"))

assocDynList = #(assoc-pos-dyn
 "1 mp#2.5#-2" 'clB / "1 p#3.5#-1" 'timbales / "(1 8) mp#2#-2" cls
 "11 mf" 'piccolo / "12 mf" (if (part? 'score) fls 'fl1) /
 "11 f#4#-1" (lst cls 'bsn tps) / "11 mp#2#-1" '(saxB ctb timbales) / "(11 8) mp#2#-1" altTen
 "C mf" (lst-diff all 'xylo tbs) ; all instruments but xylo and trombones
 "27 f / D mf" all / "D" (lst 'piccolo percus) ; a number alone delete previous dyn
 (theme1-dyn 36) '(cl1 cl2 saxT tp1) / (theme1-dyn 38) '(cl1 alt1) /
 (theme1-dyn 40) (lst fls 'cl1 'alt1) /
 (theme2-dyn D) (lst fls alts) / (theme2-dyn 37) (lst fls 'htb 'cl3 tp23) /
 (theme2-dyn 39) 'tp1 / (theme2-dyn 41) 'cor1
 "(36 -4) dimtext" (lst cl12 'saxT 'tp1) / "(36 4) dimtext" '(clB tuba ctb saxB)
 "(36 2) dimtext" (lst fls 'htb 'cl3 alts tp23 cors tbs 'euph 'bsn)
 "38 mp / 40 p" (lst fls '(cl1 alt1 clB bsn ctb) tbs  tubas) / "38" fls
 "39 mp" (lst tps 'saxB) /  "41 p" cors
 "(38 4 8) > / (39 -8) !" (lst fls cl23 '(htb alt2 saxT) cors) /
 "40 mp / (40 4 8) > / (41 -8) !" (lst cl23 '(htb alt2))
 ((set-dyn "(43 4) ~acrescPocoAPoco") (if (part? 'score) "^" "_")) ; UP only for score
     (lst cors '(ctb saxB clB bsn percu xylo) tubas tbs)
 "(44 4) cresc" '(cl3 saxT) / "(45 -4) cresc / (45 2) !" 'cl2 /
 "(45 8) cresc##-0.6" alts / "(45 2) !" (lst cors altTen 'cl3)
)
% dynamics from m.42 to m.46, are changing every 2 beats
#(define dyns-42 ; list of pos-dyn strings
   (map (set-dyn "~a ~a")
        (x-pos 42 46 '(n (n 2))) ; x-pos make a list of pos from a pattern
        (string-split "pp p#3#-2.5 p mp mp mf f#2 sf" #\space)))
% #(write dyns-42)
#(define instrus-42 ; the instruments associated with each pos-dyn of dyns-42
   (list (lst cors '(ctb saxB clB bsn cl1 percu) tubas tbs) ; 42 pp
         '(tp3 cl3 xylo)                ; '(42 2) p
         (lst '(fl1 cl2 tp2) alts)      ; 43
         '(fl2 cl1 tp1)                 ; '(43 2)
         (lst 'fl1 altTen)              ; 44
         '(fl2 htb cl1 cl2)             ; '(44 2)
         (lst '(piccolo fl1) alts)      ; 45
         (lst-diff all cl23 altTen cors 'timbales))) % '(45 2)
% the order of elts in assocDynList has no importance, we can use cons (and fold)
% instead of append.
assocDynList = #(fold cons          ; = add each pair to assocDynList.
                      assocDynList  ; original list
                      (map cons dyns-42 instrus-42)) % list of pairs to add

%}

%%%%%%
% step 9 : the separate parts
% You can go to the sub directory : Parts, and compile all parts (lilypond *.ly)


%%%%%%
% step 10 : TESTING CODE

                        %%%%% Partial compilation %%%%%
%% Uncomment the following line "show-score" to compile only from measure 10 to 15
%% You'll get probably some error messages, the same you get using { \set Score.skipTypesetting = ##t }
%% but the right section is displayed.

% #(show-score 10 15)

                       %%%%% Instruments EXPORT %%%%%%
%% Uncomment the following line "export-instruments" and you'll get an new file
%% "export.ly", in the same directoy as this current file.
%% All instruments will be defined in the traditional way : instru = { ...}
%% Use Frescobaldi functions to for example, convert notes from absolute pitch to
%% relative pitch, or to change the language

% #(export-instruments all "export.ly" #t) %% Attention : #t => overwrite the previous export.ly
