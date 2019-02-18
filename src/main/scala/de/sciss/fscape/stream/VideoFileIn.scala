/*
 *  VideoFileIn.scala
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

package de.sciss.fscape
package stream

import java.awt.image.{BufferedImage, DataBuffer}

import akka.stream.Attributes
import akka.stream.stage.OutHandler
import de.sciss.file._
import de.sciss.fscape.stream.impl.{BlockingGraphStage, NodeImpl, UniformSourceShape}
import org.bytedeco.javacv.{FFmpegFrameGrabber, Java2DFrameConverter}

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}

// XXX TODO make it so that ImageFileInImpl could be used here
object VideoFileIn {
  def apply(file: File, numChannels: Int)(implicit b: Builder): Vec[OutD] = {
    val source  = new Stage(file, numChannels = numChannels)
    val stage   = b.add(source)
    stage.outlets.toIndexedSeq
  }

  private final val name = "VideoFileIn"

  private type Shape = UniformSourceShape[BufD]

  // similar to internal `UnfoldResourceSource`
  private final class Stage(file: File, numChannels: Int)(implicit ctrl: Control)
    extends BlockingGraphStage[Shape](s"$name(${file.name})") {

    val shape: Shape = UniformSourceShape(
      outlets = Vector.tabulate(numChannels)(ch => OutD(s"$name.out$ch"))
    )

    def createLogic(attr: Attributes) = new Logic(shape, file, numChannels = numChannels)
  }

  private final class Logic(shape: Shape, file: File, protected val numChannels: Int)(implicit ctrl: Control)
    extends NodeImpl(s"$name(${file.name})", shape)
      with OutHandler {

    private[this] val outlets: Vec[OutD] = shape.outlets.toIndexedSeq

    private[this] var grabber: FFmpegFrameGrabber = _
    private[this] val converter = new Java2DFrameConverter

//    private[this] var _canRead      = false
    private[this] var inputsEnded   = false
    private[this] var outOff        = 0
    private[this] var outRemain     = ctrl.blockSize
    private[this] var framesRemain  = 0

    // ---- impl ----

    private[this]   val bufOuts     : Array[BufD]     = new Array(numChannels)
    private[this]   var numBands    : Int             = _
    private[this]   var numFrames   : Int             = _
    private[this]   var framesRead  : Int             = _
    private[this]   var gain        : Double          = _
    private[this]   var pixBuf      : Array[Double]   = _
    private[this]   var img         : BufferedImage   = _

    shape.outlets.foreach(setHandler(_, this))

//    override def preStart(): Unit = {
//      logStream(s"preStart() $this")
//    }

    override def preStart(): Unit = {
      logStream(s"preStart() $this")
      grabber = new FFmpegFrameGrabber(file.path)
      grabber.start()
    }

    private def openImage(): Boolean = {
      closeImage()
//      val frame = grabber.grab()
      val frame = grabber.grabFrame(/* doAudio = */ false, /* doVideo = */ true, /* processImage */ true, /* keyFrames */ false)
      if (frame == null) return false

      img         = converter.getBufferedImage(frame)
      numBands    = img.getSampleModel.getNumBands
      if (numBands != numChannels) {
        Console.err.println(s"Warning: ImageFileIn - channel mismatch (file has $numBands, UGen has $numChannels)")
      }
      numFrames   = img.getWidth * img.getHeight
//      println(s"openImage: ${img.getWidth()} x ${img.getHeight()}")
      val bufSize = numBands * img.getWidth
      pixBuf      = new Array(bufSize)

      val gainR = img.getSampleModel.getDataType match {
        case DataBuffer.TYPE_BYTE   =>   255.0
        case DataBuffer.TYPE_USHORT => 65535.0
        case DataBuffer.TYPE_FLOAT  =>     1.0
      }
      gain = 1.0 / gainR

      framesRead  = 0
      true
    }

    private def closeImage(): Unit = {
      pixBuf = null
      if (img != null) {
        img.flush()
        img = null
      }
    }

    override protected def stopped(): Unit = {
      logStream(s"postStop() $this")
      grabber.stop()
      freeOutputBuffers()
      closeImage()
    }

    private def freeOutputBuffers(): Unit = {
      var i = 0
      while (i < bufOuts.length) {
        if (bufOuts(i) != null) {
          bufOuts(i).release()
          bufOuts(i) = null
        }
        i += 1
      }
    }

    private def read(x: Int, y: Int, width: Int, offIn: Int): Int = {
      val offOut  = offIn + width
      img.getRaster.getPixels(x, y, width, 1, pixBuf)
      var ch  = 0
      val a   = pixBuf
      val nb  = numBands
      val g   = gain
      while (ch < numChannels) {
        val out = outlets(ch)
        if (!isClosed(out)) {
          if (bufOuts(ch) == null) bufOuts(ch) = control.borrowBufD()
          val bufOut  = bufOuts(ch)
          val b       = bufOut.buf
          if (ch < nb) {
            var i = ch
            var j = offIn
            while (j < offOut) {
              b(j) = a(i) * g
              i   += nb
              j   += 1
            }
          } else {
            Util.clear(b, offIn, width)
          }
        }
        ch += 1
      }
      offOut
    }

    protected def processChunk(outOff: Int, chunk: Int): Unit = {
      val stop  = framesRead + chunk
      val w     = img.getWidth
      val x0    = framesRead % w
      val y0    = framesRead / w
      val x1    = stop       % w
      val y1    = stop       / w

      // first (partial) line
      var off0 = read(
        x       = x0,
        y       = y0,
        width   = (if (y1 == y0) x1 else w) - x0,
        offIn   = outOff
      )

      // middle lines
      var y2    = y0 + 1
      while (y2 < y1) {
        off0 = read(
          x       = 0,
          y       = y2,
          width   = w,
          offIn   = off0
        )
        y2 += 1
      }

      // last (partial) line
      if (y1 > y0 && x1 > 0) read(
        x       = 0,
        y       = y1,
        width   = x1,
        offIn   = off0
      )

      framesRead += chunk
    }

    protected def writeOuts(chunk: Int): Unit = {
      var ch = 0
      while (ch < numChannels) {
        val out     = outlets(ch)
        val bufOut  = bufOuts(ch)
        if (bufOut != null) {
          if (isClosed(out)) {
            println(s"Wowowo - $out closed")
            bufOut.release()
          } else {
            if (chunk > 0) {
              bufOut.size = chunk
              push(out, bufOut)
            } else {
              bufOut.release()
            }
          }
          bufOuts(ch) = null
        }
        ch += 1
      }
    }

    override def onDownstreamFinish(): Unit =
      if (shape.outlets.forall(isClosed(_))) {
        logStream(s"completeStage() $this")
        completeStage()
      }

    private def canWrite: Boolean =
      shape.outlets.forall(out => isClosed(out) || isAvailable(out))

    override def onPull(): Unit =
      if (numChannels == 1 || canWrite) process()

    @tailrec
    private def process(): Unit = {
      logStream(s"process() $this")
      var stateChange = false

      if (framesRemain == 0 && !inputsEnded) {
        val ok        = openImage()
        if (ok) {
          framesRemain  = numFrames
        } else {
          inputsEnded   = true
        }
        stateChange   = true
      }

      val chunk = math.min(outRemain, framesRemain)

      if (chunk > 0) {
        processChunk(outOff = outOff, chunk = chunk)
        outOff       += chunk
        outRemain    -= chunk
        framesRemain -= chunk
        stateChange   = true
      }

      val flushOut = framesRemain == 0 && inputsEnded

      if ((outRemain == 0 || flushOut) && canWrite) {
        if (outOff > 0) {
          writeOuts(outOff)
          outOff      = 0
          outRemain   = ctrl.blockSize
          stateChange = true
        }

        if (flushOut) {
          logStream(s"completeStage() $this")
          completeStage()
          stateChange = false
        }
      }

      if (stateChange) process()
    }

//    private def updateCanRead(): Unit =
//      _canRead = isAvailable(in0)
  }
}