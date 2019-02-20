/*
 *  Main.scala
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
import de.sciss.lucre.synth.{InMemory, Server}
import de.sciss.osc
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.io.AudioFile
import de.sciss.synth.{ServerConnection, Server => SServer}

import scala.concurrent.{Future, Promise}
import scala.util.Try
import scala.util.control.NonFatal

object Main {
  type S = InMemory

  private def buildInfString(key: String): String = try {
    val clazz = Class.forName("de.sciss.muwa.BuildInfo")
    val m     = clazz.getMethod(key)
    m.invoke(null).toString
  } catch {
    case NonFatal(_) => "?"
  }

  def name              : String      = buildInfString("name")
  final def version     : String      = buildInfString("version")
  final def builtAt     : String      = buildInfString("builtAtString")
  final def fullVersion : String      = s"v$version, built $builtAt"

  def nameVersion: String = s"$name $version"

  def main(args: Array[String]): Unit = {
    Config.parse(args).fold(sys.exit(1)) { implicit config =>
      println(s"$name - $fullVersion")
      cpuLimit()
      wipeTemp()
      val pool0 = drainSoundPool()
      val atmo0 = initAtmo()
//      val futServer = bootServer()
//      futServer.foreach { implicit s =>
        implicit val system: S = InMemory()
        system.step { implicit tx =>
          val player  = SoundPlayer   .run(pool0)
          val control = Control       .run(player)
          /* val capture = */ Capture .run(control, player, atmo0)
        }
//      }
    }
  }

  def initAtmo()(implicit config: Config): Vec[File] =
    config.fAtmoDir.children(_.ext == "aif")

  def mkTemp()(implicit config: Config): Unit =
    config.fTempDir.mkdirs()

  def wipeTemp()(implicit config: Config): Unit = {
    mkTemp()
    config.fTempDir.children.foreach(_.delete())
  }

  def mkSoundPool()(implicit config: Config): Unit =
    config.fSoundPoolDir.mkdirs()

  def drainSoundPool()(implicit config: Config): Vec[File] = {
    mkSoundPool()
    val all = config.fSoundPoolDir.children
    val (keep0, remove0) = all.partition { f =>
      Try(AudioFile.readSpec(f)).toOption.exists(_.numFrames > 0)
    }
    val (keep, remove1) = keep0.splitAt(config.soundPoolSz)
    val remove = remove0 ++ remove1
    println(s"Initial sound pool: keeping ${keep.size}, removing ${remove.size}")
    remove.foreach(_.delete())
    keep
  }

  def cpuLimit()(implicit config: Config): Unit =
    if (config.cpuLimit > 0) {
      // cf. https://stackoverflow.com/questions/35842/how-can-a-java-program-get-its-own-process-id
      try {
        val pid     = java.lang.Integer.parseInt(file("/proc/self").getCanonicalFile.name)
        val pidS    = pid.toString
        val limitS  = config.cpuLimit.toString
        println(s"Setting CPU limit process $pidS to $limitS%")
        val program = "cpulimit"
        val args = Seq[String](
          "-l", limitS,
          "-p", pidS
        )
        import sys.process._
        val p = Process(program, args)
//        val code = p.!
        p.run()
//        val msg = if (code == 0) "Done." else s"$program returned with code $code"
//        println(msg)

      } catch {
        case NonFatal(ex) =>
          println("Could not set CPU limit:")
          ex.printStackTrace()
      }
    }

  def bootServer()(implicit config: Config): Future[Server] = {
    val sCfg                = Server.Config()
    sCfg.transport          = if (config.soundUseTCP) osc.TCP else osc.UDP
    sCfg.inputBusChannels   = 0
    sCfg.outputBusChannels  = 4
    sCfg.memorySize         = 16384 // 32768
    sCfg.deviceName         = Some(config.jackClientName)
    val p = Promise[Server]()
    SServer.boot(config = sCfg) {
      case ServerConnection.Running(peer) =>
        p.trySuccess(Server(peer))
    }
    p.future
  }
}
