# MuWa

[![Build Status](https://travis-ci.org/Sciss/MuWa.svg?branch=master)](https://travis-ci.org/Sciss/MuWa)

## statement

This is a small sound installation.
It is (C)opyright 2019 by Hanns Holger Rutz. All rights reserved. This project is released under 
the [GNU General Public License](https://raw.github.com/Sciss/JavaCVTest/master/LICENSE) v3+ and comes with absolutely 
no warranties. To contact the author, send an email to `contact at sciss.de`.

## requirements / installation

This project compiles against Scala 2.12 using sbt.

## tests

    sbt 'runMain de.sciss.muwa.PhaseAnalysis --video-input /home/pi/Videos/bla.h264 --audio-output /home/pi/Music/bla-synth.aif -w 960 -h 540 -n 2000 -s 22'
