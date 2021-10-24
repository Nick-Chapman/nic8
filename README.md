# nic8

# 8-bit breadboard computer

Based on the awesome video series [Build an 8-bit computer from scratch](https://eater.net/8bit/)
by Ben Eater.

This repo contains various helper files for the implementation of my take on _SAP_ - A simple as possible 8 bit architecture.

## Architecture

_TODO_ : Summary of the differences between my architecture and the original Ben Eater architecture.

### Haskell assembler & emulator

- [Op Codes](src/Op.hs)
- [Emulator](src/Emu.hs)
- [Assembler](src/Asm.hs)
- [Regression tests](src/Test.hs)
- [Example programs](src/Examples.hs)
- [Program to generate prime numbers](src/Primes.hs)

The design/implementation for the control logic is expressed in a
somewhat Verilog style [here](control-logic-design.txt).

### Roms

There's some haskell code [here](src/Rom2k.hs) to aid in the the generation of the 2k roms. We use roms in two way:

- for decoding bytes on LEDs
- to hold 8 programs which can be loaded into Ram.

There's some arduino code [here](arduino/eeprom/eeprom.ino) to drive my home build EEPROM programmer. This is similar to [Ben's design](https://www.youtube.com/watch?v=K88pgWhEb1M&list=PLowKtXNTBypGqImE405J2565dvjafglHU&index=32) except that I use a counter to control the address lines instead of a shift register.

Once I moved on to the _6502_ project, I switched to using a [commercial EEPROG programmer(https://smile.amazon.co.uk/gp/product/B07DK3NCM2), for speed, convenience and reliability.


# 6502 computer

Based on another awesome video series [Build a 6502 computer](https://eater.net/6502/) by Ben Eater.

Ben's design includes:

- CPU _65c02_
- VIA (Versatile interface adapter) _65c22_
- 256K ROM _28C256_
- 256K SRAM _62256_
- LCD controller _HD44780_

In addition I added the Texas Instruments _76489_ sound chip, which was a lot of fun!

My 6502 code is collected [here](6502). Also there is some [haskell code](src/Arc.hs) to help prepare [some Arcadian's sound data](6502/arc-data.s) used by [my 6502 playback code](6502/arcadians.s) which drives the 76489 sound chip.

The [original data](src/ArcRawData.hs) was captured by making a small mod to my local copy of the [_b-em_](http://b-em.bbcmicro.com/) BBC micro emulator, following an idea I read on
[The Oddbloke Geek Blog](http://danceswithferrets.org/geekblog/?p=93).

I also wrote a [player](6502/sonic.s) for some _Sonic the Hedgehog_ tunes which I found on the [bitshifters github](https://github.com/bitshifters/beeb-tracker).
