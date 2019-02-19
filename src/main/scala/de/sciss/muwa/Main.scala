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
import de.sciss.osc
import de.sciss.synth.{Server, ServerConnection}

import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

object Main {
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
      wipeTemp()
      val futServer = bootServer()
    }
  }

  def mkTemp()(implicit config: Config): Unit =
    config.fTempDir.mkdirs()

  def wipeTemp()(implicit config: Config): Unit = {
    mkTemp()
    config.fTempDir.children.foreach(_.delete())
  }

  def bootServer()(implicit config: Config): Future[Server] = {
    val sCfg                = Server.Config()
    sCfg.transport          = osc.TCP
    sCfg.inputBusChannels   = 0
    sCfg.outputBusChannels  = 4
    sCfg.deviceName         = Some(config.jackClientName)
    val p = Promise[Server]()
    Server.boot(config = sCfg) {
      case ServerConnection.Running(s) =>
        p.trySuccess(s)
    }
    p.future
  }
}
