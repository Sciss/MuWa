/*
 *  Capture.scala
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

import java.util.{Timer, TimerTask}

import de.sciss.osc
import de.sciss.file._
import de.sciss.synth.UGenSource.Vec

import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/*
    - record video to temp
    - render sound
    - delete video
    - inject sound
    - repeat

 */
object Capture {
  type S = Main.S

  def run(control: Control, player: SoundPlayer, atmo: Vec[File])(implicit config: Config, system: S, tx: S#Tx): Capture = {
    val res = new Impl(control, player, atmo)

    tx.afterCommit {
      res.iterate()
    }

    res
  }

  private class Impl(control: Control, player: SoundPlayer, atmo: Vec[File])(implicit config: Config, system: S)
    extends Capture {

    private[this] val fscapeCfg = PhaseAnalysis.mkFScapeConfig()
    private[this] val timer     = new Timer("capture", false)

    private def deleteVideoAndIterate(fVideo: File): Unit = {
      fVideo.delete()
      reIterate()
    }

    private def reIterate(): Unit =
      timer.schedule(new TimerTask {
        def run(): Unit = iterate()
      }, 10000L)

    private def printAndReport(ex: Throwable, op: String): Unit = {
      println(s"Failed to $op")
      ex.printStackTrace()
      control.reportBack(osc.Message("/fail", op, Option(ex.getMessage).getOrElse(ex.getClass.getName)))
    }

    def iterate(): Unit = {
      println("Iterating")
      val futVideo = TakeVideo.run()
      futVideo.onComplete {
        case Success(fVideo) =>
          val fAudioOut = File.createTempIn(config.fSoundPoolDir, prefix = "pool", suffix = ".aif", deleteOnExit = false)
          val atmoIdx   = util.Random.nextInt(atmo.size)
          val fAtmo     = atmo(atmoIdx)
          println(s"Atmo: ${fAtmo.name}")
          val config1   = config.copy(
            fVideoIn  = fVideo,
            fAudioIn  = fAtmo,
            fAudioOut = fAudioOut
          )
          val futPhase  = PhaseAnalysis.run()(fscapeCfg, config1)
          futPhase.onComplete {
            case Success(_) =>
              system.step { implicit tx =>
                player.addToPool(fAudioOut)
              }
              deleteVideoAndIterate(fVideo)

            case Failure(ex) =>
              printAndReport(ex, "video-record")
              deleteVideoAndIterate(fVideo)
          }

        case Failure(ex) =>
          printAndReport(ex, "video-record")
          reIterate()
      }
    }
  }
}
trait Capture