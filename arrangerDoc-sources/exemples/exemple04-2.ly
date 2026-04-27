%% example 4-1
\version "2.24.0"

\include "exemple04-NOTES.ily"

#(rm 'clar '(3 -2.) #{ a'2. #})

%(rm 'clar `(2 ,(ly:music-length cadenza)) #{ a'2. #})

\new Staff { << \global \clar >> }
 
