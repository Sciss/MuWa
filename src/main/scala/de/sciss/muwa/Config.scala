/*
 *  Config.scala
 *  (MuWa)
 *
 *  Copyright (c) 2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.muwa

import java.net.InetAddress

import de.sciss.file._
import de.sciss.numbers.Implicits._

object Config {
  def default: Config = {
    val workLaptop  = file("/data/temp")
    val isLaptop    = workLaptop.isDirectory
    val videoDir    = if (isLaptop) workLaptop else userHome / "Videos"
    //    val imageDir    = if (isLaptop) workLaptop else userHome / "Pictures"
    val musicDir    = if (isLaptop) workLaptop else userHome / "Music"
    val poolDir     = musicDir / "muwa"
    val atmoDir     = if (isLaptop) file("/data/projects/Almat/events/impuls2019/audio_work/atmo") else musicDir / "atmo"

    Config(
      fVideoIn      = videoDir / "bla3.h264",
      //      tempImageOut  = imageDir / "bla-%d.jpg",
      fAudioOut     = musicDir / "test.aif",
      fAudioIn      = atmoDir  / "rain-testCutRsmp.aif",
      fSoundPoolDir = poolDir,
      fAtmoDir      = atmoDir,
      isLaptop      = isLaptop
    )
  }

  def parse(args: Array[String], name: String = Main.nameVersion): Option[Config] = {
    val default = Config.default

    val p = new scopt.OptionParser[Config](name) {
      opt[File]('i', "video-input")
        .text("Input video file in phase analysis")
        .action { (f, c) => c.copy(fVideoIn = f) }

      opt[File]('o', "output")
        .text("Output jpg template - use %d as frame number place holder")
        .action { (f, c) => c.copy(tempImageOut = Some(f)) }

      opt[File]("audio-input")
        .text("Input audio file")
        .action { (f, c) => c.copy(fAudioIn = f) }

      opt[File]("audio-output")
        .text("Output audio file in phase analysis")
        .action { (f, c) => c.copy(fAudioOut = f) }

      opt[Int]('w', "width")
        .text("Video/image width in pixels")
        .action { (v, c) => c.copy(width = v) }

      opt[Int]('h', "height")
        .text("Video/image height in pixels")
        .action { (v, c) => c.copy(height = v) }

      opt[Int]('n', "num-frames")
        .text("Number of frames to render when using image sequence template")
        .action { (v, c) => c.copy(numFrames = v) }

      opt[Double]('p', "impulse-len")
        .text(s"Length of impulses for convolution in seconds (default: ${default.irDur})")
        .action { (v, c) => c.copy(irDur = v) }

      opt[Double]('l', "input-len")
        .text(s"Length of input chunks for convolution in seconds (default: ${default.inputStepDur})")
        .action { (v, c) => c.copy(inputStepDur = v) }

      opt[Int]('s', "impulse-step")
        .text(s"Interpolation steps for convolution of adjacent impulses (default: ${default.irSteps})")
        .action { (v, c) => c.copy(irSteps = v) }

      opt[Double]('g', "gain")
        .text(s"Gain in decibels (default: ${default.gain.ampDb} dB)")
        .action { (v, c) => c.copy(gain = v.dbAmp) }

      opt[String]("jack-client")
        .text(s"Jack client name (default: ${default.jackClientName})")
        .action { (v, c) => c.copy(jackClientName = v) }

      opt[Double]("video-dur")
        .text(s"Video capture duration in seconds (default: ${default.videoDur})")
        .action { (v, c) => c.copy(videoDur = v) }

      opt[Int]("video-fps")
        .text(s"Video capture frames per second (default: ${default.videoFPS})")
        .action { (v, c) => c.copy(videoFPS = v) }

      opt[Int]("video-skip")
        .text(s"Video capture skip frames (default: ${default.videoSkip})")
        .action { (v, c) => c.copy(videoSkip = v) }

      opt[Double]("video-dur")
        .text(s"Video capture duration in seconds (default: ${default.videoDur})")
        .action { (v, c) => c.copy(videoDur = v) }

      opt[Double]("sound-interval")
        .text(s"Sound playback interval in seconds (default: ${default.soundInterval})")
        .action { (v, c) => c.copy(soundInterval = v) }

      opt[File]("sound-pool")
        .text(s"Sound pool directory (default: ${default.fSoundPoolDir})")
        .action { (v, c) => c.copy(fSoundPoolDir = v) }

      opt[Int]("sound-pool-num")
        .text(s"Number of files in sound pool (default: ${default.soundPoolSz})")
        .action { (v, c) => c.copy(soundPoolSz = v) }

      opt[Double]("master-gain")
        .text(s"Master gain in decibels (default: ${default.masterAmp.ampDb} dB)")
        .action { (v, c) => c.copy(masterAmp = v.dbAmp) }

      opt[Int]("local-osc-port")
        .text(s"Raspberry Pi's OSC port (default: ${default.localOscPort})")
        .action { (v, c) => c.copy(localOscPort = v) }

      opt[Int]("control-osc-port")
        .text(s"Remote OSC port, on IP 192.168.0.77 (default: ${default.controlOscPort})")
        .action { (v, c) => c.copy(controlOscPort = v) }

      opt[Unit]("no-dump-osc")
        .text("Do not dump OSC messages")
        .action { (_, c) => c.copy(dumpOSC = false) }

      opt[File]("atmo-dir")
        .text(s"Sound atmo directory (default: ${default.fAtmoDir})")
        .action { (f, c) => c.copy(fAtmoDir = f) }

      opt[Unit]("jessie")
        .text("Run Debian 8 compatible")
        .action { (_, c) => c.copy(isJessie = true) }

      opt[Unit]("sound-tcp")
        .text("Use TCP for sound server instead of UDP")
        .action { (_, c) => c.copy(soundUseTCP = true) }

      opt[Int]("cpu-limit")
        .text(s"Limit JVM CPU usage to percentage (default: ${default.cpuLimit})")
        .action { (v, c) => c.copy(cpuLimit = v) }
    }
    p.parse(args, default)
  }

  def thisIP(): String = {
    import sys.process._
    // cf. https://unix.stackexchange.com/questions/384135/
    val ifConfig    = Seq("ip", "a", "show", "eth0").!!
    val ifConfigPat = "inet "
    val line        = ifConfig.split("\n").map(_.trim).find(_.startsWith(ifConfigPat)).getOrElse("")
    val i0          = line.indexOf(ifConfigPat)
    val i1          = if (i0 < 0) 0 else i0 + ifConfigPat.length
    val i2          = line.indexOf("/", i1)
    if (i0 < 0 || i2 < 0) {
      val local = InetAddress.getLocalHost.getHostAddress
      Console.err.println(s"No assigned IP4 found in eth0! Falling back to $local")
      local
    } else {
      line.substring(i1, i2)
    }
  }
}
case class Config(
                   fVideoIn       : File,
                   fAudioOut      : File,
                   fAudioIn       : File,
                   fSoundPoolDir  : File,
                   fAtmoDir       : File,
                   fTempDir       : File          = file("/dev/shm/muwa"),
                   tempImageOut   : Option[File]  = None,
                   width          : Int           = 960,
                   height         : Int           = 540,
                   numFrames      : Int           = 10,
                   irDur          : Double        = 0.4,
                   inputStepDur   : Double        = 0.04,
                   irSteps        : Int           = 9, // 25,
                   sampleRate     : Double        = 48000.0,
                   gain           : Double        = 30.dbAmp,
                   videoDur       : Double        = 64.0,
                   videoFPS       : Int           = 4,
                   videoSkip      : Int           = 16,
                   isLaptop       : Boolean       = false,
                   jackClientName : String        = "MuWa",
                   soundInterval  : Double        = (4 * 60.0 + 33.0)/2,
                   soundPoolSz    : Int           = 20,
                   masterAmp      : Double        = 12.dbAmp,
                   localOscPort   : Int           = 18979,
                   controlOscPort : Int           = 18980,
                   dumpOSC        : Boolean       = true,
                   isJessie       : Boolean       = false,
                   soundUseTCP    : Boolean       = false,
                   cpuLimit       : Int           = -1
                 )
