/*
 *  TakeVideo.scala
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

import java.io.IOException

import de.sciss.file._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object TakeVideo {
  def main(args: Array[String]): Unit = {
    val opt = Config.parse(args, name = "TakeVideo")
    opt.fold(sys.exit(1)) { implicit config =>
      Main.mkTemp()
      val futFile = run()
      futFile.onComplete {
        case Success(f) =>
          println(s"Video: $f")
          sys.exit()
        case Failure(ex) =>
          ex.printStackTrace()
          sys.exit(1)
      }
    }
  }

  def run()(implicit config: Config): Future[File] = {
    if (config.isLaptop) {
      val f = file("/data/projects/Almat/events/impuls2019/video_work/bla3.h264")
      Future.successful(f)
    } else {
      val f       = File.createTempIn(parent = config.fTempDir, suffix = ".h264")
      val program = "raspivid"
      val args    = Seq[String](
         "-w"   , config.width.toString,
        "-h"    , config.height.toString,
        "--ISO" , "400",
        "-ex"   , "beach",
        "-rot"  , "180",
        "-fli"  , "off",
        "-awb"  , "sun",
        "-drc"  , "off",
        "-t"    , (config.videoDur * 1000).toInt.toString,
        "-fps"  , config.videoFPS.toString,
        "-n"    , // no-preview
        "-o"    , f.path
      )
      import sys.process._
      val p = Process(program, args)
      Future {
        val code = p.!
        if (code == 0) f else throw new IOException(s"$program returned $code")
      }
    }
  }
}
