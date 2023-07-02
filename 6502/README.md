
# 6502 computer

This project is essentially independent from the enclosing `nic8` project.

There's lots of interesting things to write about given the time!

Including the garbage collected multi-processing framework.


### Commands

Flashing a rom:
```
minipro -p AT28C256 -w <rom-image>
```

Serial communication:
```
picocom -b 19200 --imap lfcrlf /dev/ttyUSB0
```
(To exit: `C-aC-x`)
