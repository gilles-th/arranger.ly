\version "2.24.0"

%%%%%%%%%%%%%%%%%%%%%% version Y/M/D = 2022/12/28 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LSR = http://lsr.di.unimi.it/LSR/Item?u=1&id=761
%% LSR = http://lsr.di.unimi.it/LSR/Item?u=1&id=545
%% for Lilypond 2.20 or higher.
%% NOTES
%  - The old name of chordsAndVoices.ly was chord.ly
%  - chordsAndVoices.ly now calls checkPitch.ly because
%    they shared some functions
%% Last major change in extract-note function : as \tuplet as now
%% a 'duration property, we have to deal with this special case.
%% by chord->note. You can now specified several numbers, to
%% extract several notes at one time

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           CHORDS STAFF
\include "checkPitch.ly"

#(define (name-of music)
 (ly:music-property music 'name))

#(define (noteEvent? music)
(eq? (name-of music) 'NoteEvent))

#(define (get-dur note)
(let ((dur (ly:music-property note 'duration)))
  (and (ly:duration? dur)
       dur)))
% #(define (get-dur note) ;; is it equivalent from above ?
% (ly:music-property note 'duration #f)

#(define (set-dur note dur)
(ly:music-set-property! note 'duration dur)
note)

#(define (expand-notes-and-chords music) ; resolves { c2.~ 4 <c e>2.~ q4 }
"Makes sure that all notes have a pitch"
(expand-repeat-notes! (expand-repeat-chords! (list 'rhythmic-event)  music)))

%%%%%%%%%%%%%%%%%%%%%%%%%%  extractNote  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define tagNotExtractNote (gensym))
#(use-modules (ice-9 receive)) %% for the use of receive

#(define (chord->note chord n . args)
"Return either the note n of chord chord, keeping articulations or if other
numbers are specified in args, a chord with the matching notes."
(receive (notes others)
 (partition noteEvent? (ly:music-property chord 'elements))
 (if (null? notes)
   chord
   (let* ((len (length notes))
          (res (filter-map
            (lambda(i)
              (and (integer? i)
                   (<= i len)
                   (> i 0)
                   (list-ref notes (1- i)))) ; list-ref is zero-based
            (cons n args)))
          (one-note (cond
             ((null? res) (list-ref notes (1- len))) ; last note
             ((null? (cdr res))(car res)) ; only one note
             (else #f))))
      (if one-note
        (begin ; not a chord, a note alone.
          (ly:music-set-property! one-note 'articulations
            (append (ly:music-property one-note 'articulations) others))
          one-note)
        (make-event-chord (append res others)))))))

#(define (extract-note music n . args)
"Extract the note n of each chords in music, keeping articulations.
If other numbers are given in args, the function returns a chord build with all
matching notes. If no note matches, returns the last note of the chord."
 (map-some-music
   (lambda (evt)
     (let ((name (name-of evt)))
      (cond
        ((eq? 'EventChord name)
           (let ((tags (ly:music-property evt 'tags)))
              (if (memq tagNotExtractNote tags)
                 (ly:music-set-property! evt 'tags ; only remove the tag
                     (delq tagNotExtractNote tags))
                 (set! evt (apply chord->note evt n args)))
              evt))
        ((eq? 'TimeScaledMusic name) #f) ;; \tuplet have now a 'duration property !
        (else (and (get-dur evt) evt))))) ; stop if note rests skip
   (expand-notes-and-chords music)))

extractNote = #(define-music-function (n music)(number? ly:music?)
 (extract-note music n))

% usefull for notExtractNote
tagify = #(define-music-function (tag music)(symbol? ly:music?)
"Add `tag in the tags property of all chords"
(music-map
  (lambda (evt)
    (if (eq? 'EventChord (name-of evt))
       (ly:music-set-property! evt 'tags
             (cons tag (ly:music-property evt 'tags))))
	evt)
  music))

notExtractNote = #(define-music-function (music)(ly:music?)
"Avoids music to be extracted by \\extractNote."
#{ \tagify #tagNotExtractNote $music #})


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% reversing chords
#(define* (reverse-chords n music #:optional (strict-comp? #t))
"Reverse n times each chord in `music, upward if n positive downward otherwise.
The lowest (resp. highest) note is repeatedly octavized until being strictly higher (resp. lower) to the highest (resp. lowest) note.
If you set strict-comp? to #f, the loop is also stopped when the 2 pitches are equal."
(if (= n 0)
  music
 (map-some-music
   (lambda(mus)
     (if (eq? 'EventChord (name-of mus))
       (receive (notes others)
         (partition noteEvent? (ly:music-property mus 'elements))
         (if (> (length notes) 1)(begin
           (for-each
             (lambda(dummy)
               (let* ((1st-note (car notes))
                      (last-note (last notes))
                      (1st-p (ly:music-property 1st-note 'pitch))
                      (last-p (ly:music-property last-note 'pitch))
                      (param (if strict-comp? 'equal 'dummy)))
                 (set! notes (if (> n 0) ;see check-pitch.ly for pitch-octavize
                   (append (cdr notes)(begin
                             (ly:music-set-property! 1st-note 'pitch
                                    (pitch-octavize 1st-p last-p 'below param))
                             (list 1st-note)))
                   (cons (begin
                            (ly:music-set-property! last-note 'pitch
                                    (pitch-octavize last-p 1st-p 'above param))
                           last-note)
                         (list-head notes (1- (length notes))))))))
             (iota (abs n)))
            (ly:music-set-property! mus 'elements (append notes others))))
          mus) ; return #t -> don't go deeper for EventChord
        #f))   ; go deeper for others mus
   (expand-notes-and-chords
     (ly:music-deep-copy music))))) % I want to use reverse-chords in scheme context => ly:music-deep-copy


reverseChords = #(define-music-function (music n) (ly:music? integer?)
(reverse-chords n music #t)
)

% threeNotes = <g b d'>4
% fourNotes = <g b d' f'>4
% \new Staff {
%     \threeNotes \reverseChords \threeNotes #1
%                 \reverseChords \threeNotes #2
%                 \reverseChords \threeNotes #3 \bar "||"
%     \fourNotes \reverseChords \fourNotes #1
%                \reverseChords \fourNotes #2
%                \reverseChords \fourNotes #3
%                \reverseChords \fourNotes #4
% }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           VOICES STAFF
%%%%%%%%%%%%%%%%%%%%%%%%%%  extractVoice  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#(define tagNotExtractVoice (gensym))

#(define (extract-voice music d deeper-level?) ;; d as a decimal number
   (define (nth-voice voices n)
    (let ((len (length voices)))
     (list-ref voices (1-   ; list-ref is zero-based !
      (if (<= n len) n len)))))

   (define (extract? x)
     (let ((tags (ly:music-property x 'tags)))
       (not (and (memq tagNotExtractVoice tags)
                 (begin
                   (ly:music-set-property! x 'tags
                       (delq tagNotExtractVoice tags))
                   #t)))))

   (define (myfilter x)
     (and (extract? x)
          (case (name-of x)
            ((SimultaneousMusic EventChord) x)
            ((OverrideProperty PropertySet VoiceSeparator) #f)
            ((ContextSpeccedMusic)
               (if (eq? (ly:music-property x 'context-type) 'Voice)
                 (set! x (myfilter (ly:music-property x 'element #f))))
               x)
            (else (if (get-dur x)
              x
              (let ((e (ly:music-property x 'element #f))
                    (es (ly:music-property x 'elements #f)))
                (if e (ly:music-set-property! x 'element (myfilter e)))
                (if (pair? es)(ly:music-set-property! x 'elements
                                 (filter myfilter es)))
                x))))))
(map-some-music
 (lambda(evt)
   (case (name-of evt)
     ((EventChord) evt)
     ((SimultaneousMusic)(if (extract? evt)
        (let* ((save-d d)                      ; if d = 4.321, we'll get :
               (n (truncate d))                ; n = 4 (the integer part)
               (next-d (- (* 10  d)(* 10 n)))  ; next-d = 43.21 - 40 = 3.21
               (voices (filter myfilter (ly:music-property evt 'elements))))
          (set! evt (if (or (null? voices)(< n 1))
             '()
             (nth-voice voices (inexact->exact n))))
          (if deeper-level? (begin
            (set! d (if (< next-d 1) n next-d))        ; keep n if (truncate next-d) = 0
            (set! evt (extract-voice evt d deeper-level?)))) ; SimultaneousMusic inside?
          (set! d save-d)))
        evt)
     (else (and (get-dur evt) evt))))
 music))

extractVoice = #(define-music-function (n music) (integer? ly:music?)
"Extract in music, the n-th voice of simultaneous music of the same level, keeping 
only basic music events (no more \\Voicexxx or \\new Voice). A Voice separator
doesn't count as a voice."
(extract-voice music n #f))

deepExtractVoice = #(define-music-function (x music) (number? ly:music?)
"Behaves like extractVoice, taking first the integer part of x as n argument, but
goes deeper in each simultaneous music, extracting voice of other potential
simultaneous music, taking now as n argument the first digit of the decimal part
of x, then continues always deeper with second digit and so on.
Notes that a digit of 0, means taking previous value of n, so 2 is equivalent to 2,222...
and 2,3 to 2,333..."
(extract-voice music x #t))

notExtractVoice = #(define-music-function (music)(ly:music?)
"When using \\extractVoice, avoids `music to be extracted."
#{ \tag #tagNotExtractVoice $music #})

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            CHORDS and VOICES together
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% extractPartUpper/Lower %%%%%%%%%%%%%%%%%%%%%%%%%
%% If you have enter << <c e g> \\ <ais cis fis> >>, the first function will
%% give you c, the second fis
extractPartUpper = #(define-music-function (music )(ly:music?)
 #{ \extractNote #1000 \extractVoice #1 $music #})

extractPartLower = #(define-music-function (music )(ly:music?)
 #{ \extractNote #1 \extractVoice #1000 $music #})
                          %%%%%% shortcuts %%%%%%%
#(define ePU extractPartUpper)
#(define ePL extractPartLower)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Miscellaneous functions
%%%%%%%%%%%%%%%%%%%%% addNote
% addNote calls add-note which calls add-note-basic
#(define (add-note-basic music notes-to-add)
"music and notes-to-add as music. 
These 2 arguments must have been expanded with expand-notes-and-chords "
  (define (note->chords-arti note)   ; note as a NoteEvent
    (receive (note-arti chord-arti)
      (partition      ; separates arti for NoteEvent from arti for EventChord
        (lambda (evt)(memq (name-of evt)
                       (list 'StringNumberEvent 'StrokeFingerEvent 'FingeringEvent)))
        (ly:music-property note 'articulations))
      (ly:music-set-property! note 'articulations note-arti)
      chord-arti))
(let* ((alist      ; a list of pairs of 2 lists : '(notes . articulations)
         (reverse (let loop ((m notes-to-add)
                             (p '())) ; m = music, p previous value of the list
           (case (name-of m)
             ((or SkipEvent SkipMusic) ; a skip in notes-to-add means : add nothing
                (cons #f p))           ; add #f to the list
             ((NoteEvent)
                (acons (list m) (note->chords-arti m) p))
             ((EventChord)
                (receive (notes arti) ; separates notes from scripts, dynamics etc
                  (partition noteEvent? (ly:music-property m 'elements))
                  (if (pair? notes)(acons notes arti p) p)))
             (else (let ((e (ly:music-property m 'element)))
                (fold loop (if (ly:music? e)(loop e p) p)
                           (ly:music-property m 'elements))))))))
       (entry #f)  ; will be (car alist)
       (entry? (lambda() (and
                 (pair? alist)
                 (begin (set! entry (car alist))
                        (set! alist (cdr alist))
                        entry))))
       (do-add (lambda (notes arti)
                 (let* ((dur (get-dur (car notes))) ; not #f with expand-notes-and-chords
                        (new-notes (map             ; fix all durations to dur
                          (lambda(evt)(set-dur evt dur))
                          (car entry)))            ; the list of new notes
                        (new-arti (cdr entry)))    ; the articulations
                   (append new-notes notes new-arti arti)))))
  ;; combine in chords, each element of alist with notes of music
  (map-some-music
    (lambda(x)
      (case (name-of x)
        ((NoteEvent)(if (entry?)
           (make-event-chord (do-add (list x) (note->chords-arti x)))
           x))
        ((EventChord)
           (if (entry?)(receive (notes arti) ; separates notes from scripts, dynamics etc
             (partition noteEvent? (ly:music-property x 'elements))
             (if (pair? notes)(ly:music-set-property! x 'elements (do-add notes arti)))))
           x)
        (else (and (get-dur x) x)))) ; #f means : go deeper
    music)))

#(define (add-note . args)
"Expands the 2 music arguments before the call of add-note-basic"
(apply add-note-basic (map expand-notes-and-chords args)))


addNote = #(define-music-function (music notes)(ly:music? ly:music?)
"Merges in a chord, the first note or chord in `music, with the first note or chord
in `notes, including articulations, then continues to the second one, and so on.
The duration of notes are taken from `music.
In `notes, only note or chord events are kept."
(add-note music notes)) % be seen in \relative mode


%%%%%%%%%%%%%%%%%%%% addVoice
%% Traditionnal way
addVoice = #(define-music-function (music newVoice) (ly:music? ly:music?)
#{ << $music \\ $newVoice >> #})


%% Alternate way
addVoiceAlt = #(define-music-function (music newVoice) (ly:music? ly:music?)
#{ << { \voiceOne $music }
      \new Voice { \voiceTwo $newVoice } >>
   \oneVoice #})

%%%%%%%%%%%%%%%%%%%%
#(define dyn-list '(AbsoluteDynamicEvent CrescendoEvent DecrescendoEvent))

deleteDynamics = #(define-music-function (music) (ly:music?)
(music-filter
  (lambda (evt)(not (memq (name-of evt) dyn-list)))
  music))

%%%%%%%%%%%%%%%%%%%%%%%%
doubleNote = #(define-music-function (music) (ly:music?)
"Double each note with the note an octave higher."
 #{ \addNote {\transpose c c' $music } $music #})

%%%%%%%%%%%%%%%%%%%%%%%%
#(define (delete-duplicates-notes music-with-chords)
"In a chord, delete a note if an another note with the same pitch is found"
(let ((chords (extract-named-music music-with-chords 'EventChord)))
   (for-each
     (lambda(chord)
       (ly:music-set-property! chord 'elements
         (delete-duplicates
          (ly:music-property chord 'elements)
          (lambda(note1 note2)
            (equal? (ly:music-property note1 'pitch #f)
                    (ly:music-property note2 'pitch #t))))))
     chords)
   music-with-chords))

#(define del-duplicate delete-duplicates-notes)

	%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%{ <- add a second % to the beginning of line to test functions
\markup \fill-line { "***** \\extractNote and \\deepExtractVoice *****" }
music = \relative c''{
  <c e g>4-> <d f>( <b g' d'>) <c e g>-.
  <<
    {e4 f g2 << {<a c>4 b c2} \new Voice { \voiceThree f,2 e} >> }
    \\
    { c2 b4 d <<{d2 c2}  \new Voice  {\voiceFour g4 r c,2}>>}
  >>
}

\score {
  \new Staff \music
  \header { piece = "Music with chords and voices"}
}
\score { <<
  \new Staff \extractNote #3 \deepExtractVoice #1 \music
  \new Staff \extractNote #2 \deepExtractVoice #1.2 \music
  \new Staff \extractNote #1 \deepExtractVoice #2.1 \music
  \new Staff \extractNote #1 \deepExtractVoice #4 \music
  >>
  \header { piece = "Music splited in 4 staffs"}
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup \fill-line { "***** \\extractNote and \\addNote *****" }

music = \relative c' {c'4.-> d8-. c4(\p b8) r c4\f c c2}
\score { <<
  \new Staff \music
  \new Staff \addNote \music \relative c' {e f e d e s e}
  >>
  \header { piece = "add {e f e d e s e}"}
}


melodie = \relative c' {
  g'2-1 g4.-2 g8-3
  a4( b)-. c-. d-.
  c2.-> r4 }
music = \addNote \addNote
           \melodie
           \relative c' { e d e f g e f e }
           \relative c' { c b c f, e a g c }
\score {<<
  \new Staff \melodie
  \new Staff \music
  \new Staff \extractNote #3 \music %% \melodie
  \new Staff \extractNote #2 \music
  \new Staff { \clef bass \extractNote #1 \music }
  >>
 \header { piece = "notes articulations vs chords articulations in add and extract operations"}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup \fill-line { "***** \\doubleNote *****" }

mus = \relative c' { a4 e' g fis g2-> c,-> }
music = \doubleNote \mus
\score {<<
  \new Staff \mus
  \new Staff \music
>>}

\pageBreak
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\markup \fill-line { "***** \\notExtractVoice *****" }

music = \relative c' \addVoiceAlt
  { \notExtractVoice \override Voice.NoteHead.color = #red
    e'2 f }
  { c2 d }

\score {<<
  \new Staff \music
  \new Staff \extractVoice #1 \music
>>}


%}