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

import de.sciss.file._
import de.sciss.numbers.Implicits._

object Config {
  def default: Config = {
    val workLaptop  = file("/data/temp")
    val isLaptop    = workLaptop.isDirectory
    val videoDir    = if (isLaptop) workLaptop else userHome / "Videos"
    //    val imageDir    = if (isLaptop) workLaptop else userHome / "Pictures"
    val soundDir    = if (isLaptop) workLaptop else userHome / "Music"

    Config(
      fVideoIn      = videoDir / "bla3.h264",
      //      tempImageOut  = imageDir / "bla-%d.jpg",
      fAudioOut     = soundDir / "test.aif",
      fAudioIn      = soundDir / "rain-testCut.aif",
      isLaptop      = isLaptop
    )
  }

  def parse(args: Array[String], name: String = Main.nameVersion): Option[Config] = {
    val default = Config.default

    val p = new scopt.OptionParser[Config](name) {
      opt[File]('i', "video-input")
        .text("Input video file")
        .action { (f, c) => c.copy(fVideoIn = f) }

      opt[File]('o', "output")
        .text("Output jpg template - use %d as frame number place holder")
        .action { (f, c) => c.copy(tempImageOut = Some(f)) }

      opt[File]("audio-input")
        .text("Input audio file")
        .action { (f, c) => c.copy(fAudioIn = f) }

      opt[File]("audio-output")
        .text("Output audio file")
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
    }
    p.parse(args, default)
  }
}
case class Config(
                   fVideoIn       : File,
                   fAudioOut      : File,
                   fAudioIn       : File,
                   fTempDir       : File          = file("/dev/shm/muwa"),
                   tempImageOut   : Option[File]  = None,
                   width          : Int           = 960,
                   height         : Int           = 540,
                   numFrames      : Int           = 11,
                   irDur          : Double        = 0.4,
                   inputStepDur   : Double        = 0.04,
                   irSteps        : Int           = 11, // 25,
                   sampleRate     : Double        = 48000.0,
                   gain           : Double        = 24.dbAmp,
                   videoDur       : Double        = 64.0,
                   videoFPS       : Int           = 4,
                   videoSkip      : Int           = 16,
                   isLaptop       : Boolean       = false,
                   jackClientName : String        = "MuWa",
                 )


