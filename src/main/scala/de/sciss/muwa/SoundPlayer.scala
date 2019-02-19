/*
 *  SoundPlayer.scala
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

import de.sciss.file._
import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.synth.{Buffer, InMemory, Server, Synth}
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.{ControlSet, SynthGraph, addAfter, addToHead, freeSelf}

import scala.concurrent.stm.Ref

/*

    schedule sound play back

 */
object SoundPlayer {
  type S = Main.S

  def run(pool0: Vec[File])(implicit s: Server, config: Config, system: S, tx: S#Tx): SoundPlayer = {
    val soundIntervalMs   = (config.soundInterval * 1000).toLong
    val soundTimer        = new Timer("sound", false)
    val master            = mkMaster()
    val task              = new Task(master, pool0)

    tx.afterCommit {
      soundTimer.schedule(task, soundIntervalMs, soundIntervalMs)
    }
    task
  }

  private def mkMaster()(implicit s: Server, config: Config, tx: S#Tx): Synth = {
    val gr = SynthGraph {
      import de.sciss.synth.Ops.stringToControl
      import de.sciss.synth.ugen._
      val amp   = "amp".kr(1f)
      val in    = In.ar(0, 4)
      val sig   = Limiter.ar(in * amp)
      ReplaceOut.ar(0, sig)
    }
    val args = List[ControlSet]("amp" -> config.masterAmp)
    Synth.playOnce(gr, nameHint = Some("master"))(s.defaultGroup, args = args, addAction = addAfter)
  }

  private class Playing(val f: File, val syn: Synth)

  private class Task(master: Synth, pool0: Vec[File])
                    (implicit s: Server, config: Config, system: S) extends TimerTask with SoundPlayer {

    private val pool      = Ref(pool0)
    private val playing   = Ref(Option.empty[Playing])

    def run(): Unit = {
      println("Sound scheduled")
      val found = system.step { implicit tx =>
        playing.swap(None).foreach { p =>
          pool.transform(p.f +: _)
          p.syn.release(4)
        }
        pool() match {
          case init :+ last =>
            val p     = play(last)
            pool()    = init
            playing() = Some(p)
            p.syn.onEndTxn { implicit tx =>
              addToPool(last, prepend = true)   // make it available again
            }
            true

          case _ =>
            false
        }
      }
      if (!found) println("(No sound in pool)")
    }

    def setMasterAmp(v: Double)(implicit tx: S#Tx): Unit =
      master.set("amp" -> v)

    private def play(f: File)(implicit tx: S#Tx): Playing = {
      val b   = Buffer.diskIn(s)(path = f.path, numChannels = 4)
      val gr = SynthGraph {
        import de.sciss.synth.Ops.stringToControl
        import de.sciss.synth.ugen._
        val bId   = "buf".kr
        val amp   = "amp".kr(1f)
        val disk  = DiskIn.ar(numChannels = 4, buf = bId, loop = 0)
        DetectSilence.ar(disk, amp = 1.0e-6f, dur = 1f, doneAction = freeSelf)
        val sig   = disk * amp
        Out.ar(0, sig)
      }
      val syn = Synth(s, gr, nameHint = Some("play"))
      val args = List[ControlSet](
        "buf" -> b.id
      )
      syn.play(s.defaultGroup, args = args, addAction = addToHead, dependencies = b :: Nil)
      syn.onEndTxn { implicit tx =>
        b.dispose()
      }
      new Playing(f = f, syn = syn)
    }

    def addToPool(f: File)(implicit tx: S#Tx): Unit = addToPool(f, prepend = false)

    private def addToPool(f: File, prepend: Boolean)(implicit tx: TxnLike): Unit = {
      val oldPool = pool()
      val (keep0, remove) = oldPool.splitAt(config.soundPoolSz - 1)
      val keep = if (prepend) f +: keep0 else keep0 :+ f
      pool() = keep
      println(s"Pool size now ${keep.size}; removing ${remove.size}")
      if (remove.nonEmpty) tx.afterCommit {
        remove.foreach(_.delete())
      }
    }
  }
}
trait SoundPlayer {
  type S = InMemory

  def addToPool(f: File)(implicit tx: S#Tx): Unit

  def setMasterAmp(v: Double)(implicit tx: S#Tx): Unit
}