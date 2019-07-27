# HMM
Haskell Music Musings

This project is intented to be experiments with creating music via Haskell.  Presently, I'm off to a slow start since I have a day job and I'm "lazy" (ha-ha!!!). No seriously, "Haskell" jokes about laziness aside, I like to do other things besides code in my free time such as "goofing off".

This first app refWave was kind of fun, but I struggled a lot with the Haskell typing system.  I guess one might be wondering why I might choose Haskell to try to create music when there are other choices out there.  Well, Haskell is what I consider to be a mathematical languages.  I feel it was designed with mathematicians in mind.  To that end, doing math calculations in Haskell seems intuitive.  So if I can get get through all of this boilerplate junk like creating wave files maybe some day I can focuse on the math of music.  That <b>might</b> be an interesting experience.

# refWave

A command line utility to take a "reasonable" frequency between 110-8000 Hz and output a sine wave in the format of a WAV file.  The generated wave file can then be used for assistend tuning of an instrument should your digital tuner be giving you trouble and you actualy want to <b>hear</b> what you are tuning to.  If you are using Windows Media player, you can loop over the WAVE so that it will play repeatedly until you are done tuning your instrument.  Unfrotunately, you need to know the frequnency of the note you are attempting to tune to.  If you are tuning to any note of "A" (i.e. A2, A3, A4, etc) this should be quite easy since 440 Hertz is a common reference pitch and you can often go up or down an octive by simply doubling or halfing the frequency of 440.  With other notes, however, it can get a bit tricky becuase I *think* the pitches within an standard scale are arranged such that there frequencies follow a logarithmic pattern.  Once you have the pitch of a note class, finding all of the ocatives of that node should be easy as the doubling / halfing trick can be used again.

To use refWave do something like this at the command line:

./refWave 440<br />
or<br />
./refWave 880<br />
<br />
You will then get a file named out.wav that you can play on your device appropriately.  

Compiline refWave is trivial since all of the packages used by refWav are already built in.  To compile refwave simply do this at the command prompt after you have properly installed GHC:

ghc ./refWave.hs



# other

I hope to some day add other projects, but I have a day job and I want to go goof off now.  This is fun, but I feel maybe Cubase calling...
