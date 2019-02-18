/*
 *  VideoFileInTest.scala
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

import de.sciss.file.{File, file}
import de.sciss.fscape.{Graph, graph, stream}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object VideoFileInTest {
  case class Config(fIn: File = file("in.264"), tempOut: File = file("out-%d.jpg"),
                    width: Int = 0, height: Int = 0, numFrames: Int = 0)

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Read Video Test") {
      opt[File]('i', "input")
        .text("Input video file")
        .required()
        .action { (f, c) => c.copy(fIn = f) }

      opt[File]('o', "output")
        .text("Output jpg template - use %d as frame number place holder")
        .required()
        .action { (f, c) => c.copy(tempOut = f) }

      opt[Int]('w', "width")
        .text("Video/image width in pixels")
        .required()
        .action { (v, c) => c.copy(width = v) }

      opt[Int]('h', "height")
        .text("Video/image height in pixels")
        .required()
        .action { (v, c) => c.copy(height = v) }

      opt[Int]('n', "num-frames")
        .text("Number of frames to render")
        .required()
        .action { (v, c) => c.copy(numFrames = v) }
    }
    p.parse(args, default).fold(sys.exit(1)) { implicit config =>
      run()
    }
  }

  def run()(implicit config: Config): Unit = {
    import config._

//    val width   = 960
//    val height  = 540
//    val fIn     = file("/data/temp/bla3.h264")
//    val tempOut = file("/data/temp/test-%d.jpg")

    val gr = Graph {
      import graph._
      val in      = VideoFileIn(file = fIn, numChannels = 3)
      val sig     = in
      val spec    = ImageFile.Spec(width = width, height = height, numChannels = 3 /* 1 */,
        fileType = ImageFile.Type.JPG, sampleFormat = ImageFile.SampleFormat.Int8)
      ImageFileSeqOut(in = sig, template = tempOut, spec = spec, indices = ArithmSeq(1, length = numFrames))
    }

    val cfg = stream.Control.Config()
  //  config.blockSize  = 599   // a prime number unrelated to `width` and `height`, for testing
    cfg.blockSize  = width
    cfg.useAsync   = false // for debugging
    val ctrl  = stream.Control(cfg)

    ctrl.run(gr)
    println("Running...")
    Await.result(ctrl.status, Duration.Inf)

  //  Swing.onEDT {
  //    SimpleGUI(ctrl)
  //  }
  }
}
