# HMM
Haskell Music Musings

This project is intented to be experiments with creating music via Haskell.  I don't spend much time on this but I goofed around with this a little on July 12, 2020 and finally make a two part music phrase consisting only of Sine Waves using the stuff from the refWave project that I coded previously.

# Cheese

This is a short Haskell program to generate a very small two part Baroque style music phrase.  The only instruments at this point are sine waves.  I used <a href="https://en.wikipedia.org/wiki/Music_tracker">Tracker</a> style notation to save the notes into two Haskell lists.  My refwave code that was written at an earlier date was used to generate the tones.  I reckon care has to be taken to ensure the Haskell lists for the two parts stay the same length and/or done get out of alignment.  Any chords that might arise are never more than two notes large.

If you are on UBuntu Studio 20.04 LTS, then you can do the following to hear it:
<ol>
  <li>cd ./HMM/apps/cheese</li>
  <li>make</li>
  <li>./cheese</li>
  <li>aplay out.wav<li>
</ol>

CAUTION: As it's name implies, this music piece is very Cheezy.  It may pop your speakers slightly and/or give you gas.  If you have gastrointestinal issues (like I do), you may want to consult with your doctor before playing this piece too many times.

OTHER: On a more serious note, I am still very upset about the naming convention of GNU\Linux background drivers.  In my oppinion Haskell is a beautiful language, but it tends to run better on GNU\Linux operatings systems than it does on MS Windows.  With that said, I wish someone would rename the GNU\Linux background drivers to something less offensive:
https://www.utilars.com/Home/LinuxBeef
As a Chrisitan, I do not want anything to do with spirits, witches, warlocks, daemons or demons.  I hope you all can undersand.  Also, Python is a rather unforutnate name for a fast growing language.  While Haskell is (in my opinion) way better than Python, many people in the Machine Learning industry have come to emrace it.  I don't claim to be any kind of angel, but please think before you pick "demonic" names for your software components.  Not all Software Engineers and/or IT professionals look at these conventions with agnostic eyes...

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

I hope to some day add other projects, but I have a day job and I want to go goof off now.  Will probably skip Cubase for today and go stright to video games...
