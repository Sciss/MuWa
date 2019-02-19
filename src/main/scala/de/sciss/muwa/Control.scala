/*
 *  Control.scala
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

import java.net.{InetAddress, InetSocketAddress}

import de.sciss.osc
import de.sciss.osc.Implicits._
import de.sciss.osc.Message

import scala.util.control.NonFatal

object Control {
  type S = Main.S

  def run(player: SoundPlayer)(implicit config: Config, system: S, tx: S#Tx): Control = {
    val res = new Impl(player)
    tx.afterCommit {
      try {
        res.init()
      } catch {
        case NonFatal(ex) =>
          println("Could not initialize control:")
          ex.printStackTrace()
      }
    }
    res
  }

  private class Impl(player: SoundPlayer)(implicit config: Config, system: S) extends Control {
    private[this] var transmitter = Option.empty[osc.UDP.Transmitter.Undirected]

    private[this] val target: InetSocketAddress = "192.168.0.77" -> config.controlOscPort

    private def received(m: osc.Message): Unit = m match {
      case osc.Message("/amp", value: Float) =>
        system.step { implicit tx =>
          player.setMasterAmp(value)
        }

      case _ =>
        println(s"Warning. Unknown OSC message $m")
    }

    def init(): Unit = {
      val cfg           = osc.UDP.Config()
      cfg.localPort     = config.localOscPort
      val localSuffix   = if (config.isLaptop) "77" else "31"
      cfg.localAddress  = InetAddress.getByName(s"192.168.0.$localSuffix")
      val t             = osc.UDP.Transmitter(cfg)
      t.connect()
      val r             = osc.UDP.Receiver(t.channel, cfg)
      r.action          = {
        case (p: osc.Message, _) => received(p)
        case _ =>
      }
      r.connect()
      if (config.dumpOSC) {
        t.dump()
        r.dump()
      }
      transmitter = Some(t)
    }

    def reportBack(m: Message): Unit = {
      transmitter.foreach { c =>
        try {
          c.send(m, target)
        } catch {
          case NonFatal(ex) =>
            println("Cannot send OSC message:")
            ex.printStackTrace()
        }
      }
    }
  }
}
trait Control {
  def reportBack(m: osc.Message): Unit
}