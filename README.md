# furNES
an NES sound driver for Furnace (pun intended)

### **THIS SOUND DRIVER IS CURRENTLY A WIP**

A Furnace sound driver that's easy-ish to use?
Yes, this is real, but you can't just drop in any .fur file with the NES chip enabled. You have to follow these rules/guidelines for your .fur file to be furNES-compatible.

* You have to have [Python](https://www.python.org/) and the [CC65 toolchain](https://cc65.github.io/) installed
* You **have** to set the pitch linearity option to "None". You can do this by going to `window -> song -> compatability flags -> Pitch/Playback -> Pitch linearity` and then setting the option to "None".

* This only supports 2A03 and FDS (which can be enabled through a flag in the driver source). Also this only supports DPCM samples, so that means that there will be no 7-bit PCM samples :/

* The driver only supports **arpeggio, volume and duty/waveform** macros in each instrument and it **DOESN'T support LFO and ADSR macros nor delay and step length**

* The furNES driver only supports these effects:
  * 01xx: pitch slide up
  * 02xx: pitch slide down
  * 03xx: portamento
  * 04xx: vibrato
  * 09xx: set speed 1
  * 0Fxx: set speed 2
  * 0Axx: volume slide
  * 11xx: set duty
  * E1xx: note slide up
  * E2xx: note slide down
  * E5xx: note fine-pitch
  * ECxx: note cut

when you've finished / want to test out this driver:
* open the terminal/command prompt **to the furNES directory**
* run `convert.sh your_fur_file.fur` or `convert.bat file.fur` (depending on your OS)
* in the `furNES/nsf` directory you'll hopefully see a file called **`furNES-test.nsf`**
  * that's your .nsf music file that you can play on any hardware or software NSF player!

Hopefully you'll have fun with my driver :D

Libraries used: chipchune
