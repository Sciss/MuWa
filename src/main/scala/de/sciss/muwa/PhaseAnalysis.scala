/*
 *  PhaseAnalysis.scala
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
import de.sciss.fscape.{GE, Graph, graph, stream}
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.AudioFileSpec

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object PhaseAnalysis {
  case class Config(
                     fVideoIn     : File,
                     tempImageOut : File,
                     fAudioOut    : File,
                     fAudioIn     : File,
                     width        : Int     = 960,
                     height       : Int     = 540,
                     numFrames    : Int     = 10,
                     irDur        : Double  = 0.5,
                     inputStepDur : Double  = 0.02,
                     irSteps      : Int     = 1, // 25,
                     sampleRate   : Double  = 48000.0
                   )

  def main(args: Array[String]): Unit = {
    val workLaptop  = file("/data/temp")
    val isLaptop    = workLaptop.isDirectory
    val videoDir    = if (isLaptop) workLaptop else userHome / "Videos"
    val imageDir    = if (isLaptop) workLaptop else userHome / "Pictures"
    val soundDir    = if (isLaptop) workLaptop else userHome / "Music"

    val default = Config(
      fVideoIn      = videoDir / "bla3.h264",
      tempImageOut  = imageDir / "bla-%d.jpg",
      fAudioOut     = soundDir / "test.aif",
      fAudioIn      = soundDir / "rain-testCut.aif"
    )

    val p = new scopt.OptionParser[Config]("Read Video Test") {
      opt[File]('i', "input")
        .text("Input video file")
        .action { (f, c) => c.copy(fVideoIn = f) }

      opt[File]('o', "output")
        .text("Output jpg template - use %d as frame number place holder")
        .action { (f, c) => c.copy(tempImageOut = f) }

      opt[Int]('w', "width")
        .text("Video/image width in pixels")
        .action { (v, c) => c.copy(width = v) }

      opt[Int]('h', "height")
        .text("Video/image height in pixels")
        .action { (v, c) => c.copy(height = v) }

      opt[Int]('n', "num-frames")
        .text("Number of frames to render")
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
    }
    p.parse(args, default).fold(sys.exit(1)) { implicit config =>
      run()
    }
  }

  def any2stringadd: Any = ()

  def run()(implicit config: Config): Unit = {
    import config._
    import graph._

    val gr = Graph {
      val videoIn   = VideoFileIn(file = fVideoIn, numChannels = 3)
      val fftSize   = (math.min(width, height) / 2).nextPowerOfTwo
      val fftSizeH  = fftSize/2
//      val fftSizeQ  = fftSizeH/2
      val mono: GE = {
        val r = videoIn.out(0)
        val g = videoIn.out(1)
        val b = videoIn.out(2)
        r * 0.2126 + g * 0.7152 + b * 0.0722
      }
      val crop: GE = {
        val dw        = width - fftSize
        val dwStart   = dw/2
        val dwStop    = -(dw - dwStart)
        val h         = ResizeWindow(mono, size = width, start = dwStart, stop = dwStop)
        val dh        = height - fftSize
        val dhStart0  = dh/2
        val dhStop0   = -(dh - dhStart0)
        val dhStart   = dhStart0 * fftSize
        val dhStop    = dhStop0  * fftSize
        val v         = ResizeWindow(h, size = fftSize * height, start = dhStart, stop = dhStop)
        v
      }

      def mkWindow(in: GE): GE = {
        val h = in *              GenWindow(size = fftSize, shape = GenWindow.Hann)
        val v = h  * RepeatWindow(GenWindow(size = fftSize, shape = GenWindow.Hann), num = fftSize)
        v
      }

      val frameSize = fftSize.squared
      // XXX TODO --- using crop directly without windowing has very interesting phase images
      val windowed  = mkWindow(crop)
      val seqA      = BufferMemory(windowed, frameSize)
      val seqB      = windowed.drop         (frameSize)
      val fftA      = Real2FullFFT(seqA, rows = fftSize, columns = fftSize)
      val fftB      = Real2FullFFT(seqB, rows = fftSize, columns = fftSize)

      val conjA     = fftA .complex.conj  // A is to be shift against B!
      val conv      = conjA.complex * fftB
      val convMagR  = conv .complex.mag.max(1.0e-06).reciprocal
      val convBuf   = BufferMemory(conv, size = frameSize)
      val elemNorm  = convBuf * RepeatWindow(convMagR)
      val iFFT0     = Real2FullIFFT(in = elemNorm, rows = fftSize, columns = fftSize)
//      val iFFT      = (iFFT0 / (fftSize * 2)) + (0.5: GE)
      val iFFT      = iFFT0 / fftSize

//      val specOut = ImageFile.Spec(width = fftSize, height = fftSize, numChannels = 1,
//        fileType = ImageFile.Type.JPG, sampleFormat = ImageFile.SampleFormat.Int8)
//      ImageFileSeqOut(in = iFFT, template = tempOut, spec = specOut, indices = ArithmSeq(1, length = numFrames))

      val phaseBase = iFFT
      val phaseDiff: GE = {
        val phaseA  = BufferMemory(phaseBase, frameSize)
        val phaseB  = phaseBase.drop         (frameSize)
//        ((phaseB absDif phaseA) * 4.0).min(1.0)
//        ((phaseB - phaseA) * 2).clip2(1.0) * 0.5 + (0.5: GE)
        phaseB - phaseA
      }

//      val phaseDiffI = (phaseDiff * 2).clip2(1.0) * 0.5 + (0.5: GE)
//      val left      = ResizeWindow(phaseDiffI, size = fftSize, stop = fftSize)
//      val right     = ResizeWindow(BufferDisk(crop).drop(frameSize), size = fftSize, start = -fftSize)
//      val composite = left + right
//
//      val specImageOut = ImageFile.Spec(width = fftSize * 2, height = fftSize, numChannels = 1,
//        fileType = ImageFile.Type.JPG, sampleFormat = ImageFile.SampleFormat.Int8)
//      ImageFileSeqOut(in = composite, template = tempImageOut, spec = specImageOut, indices = ArithmSeq(1, length = numFrames))

//      def mkAudioSig1(in: GE): GE = {
//        val hw  = in * GenWindow(size = fftSizeH, shape = GenWindow.Hann)
//        val hl  = OverlapAdd(in = hw, size = fftSizeH, step = fftSizeH/2)
//        val vw  = hl * RepeatWindow(GenWindow(size = fftSizeH, shape = GenWindow.Hann), num = fftSizeQ)
//        val vl  = OverlapAdd(in = vw, size = fftSizeH * fftSizeQ, step = (fftSizeH * fftSizeQ)/2)
//        vl
//      }

      def mkAudioSig2(in: GE): GE = {
        val i  = Real1IFFT(in, fftSizeH * fftSizeH, mode = 2)
        val w  = i * GenWindow(size = fftSizeH * fftSizeH, shape = GenWindow.Hann)
        OverlapAdd(in = w, size = fftSizeH * fftSizeH, step = (fftSizeH * fftSizeH)/4)
      }

      def mkAudioSig(q: Int): GE = {
        val hStart = if (q % 2 == 0) fftSizeH else 0
        val hStop  = if (q % 2 == 0) 0 else -fftSizeH
        val vStart = if (q / 2 == 0) fftSizeH * fftSizeH else 0
        val vStop  = if (q / 2 == 0) 0 else -fftSizeH * fftSizeH

        val h   = ResizeWindow(phaseDiff, size = fftSize, start = hStart, stop = hStop)
        val v   = ResizeWindow(h        , size = fftSizeH * fftSize, start = vStart, stop = vStop)
        mkAudioSig2(v)
      }

      val audioSig  = Seq.tabulate(4)(mkAudioSig): GE
//      val specAudioOut = AudioFileSpec(numChannels = 4,sampleRate = sampleRate)
//      AudioFileOut(in = audioSig, file = fAudioOut, spec = specAudioOut)

      val irFrames        = (irDur * sampleRate).toInt
      val irMinPhaseSz    = (irFrames * 2 - 1).nextPowerOfTwo
      val minPhaseFFTIn   = Real1FullFFT(audioSig, size = irFrames, padding = irMinPhaseSz - irFrames)
      val minPhaseLog1    = minPhaseFFTIn.complex.log.max(-320)
      val minPhaseCep     = Complex1IFFT(in = minPhaseLog1, size = irMinPhaseSz) / irMinPhaseSz
      val minPhaseCepF    = FoldCepstrum(in = minPhaseCep, size = irMinPhaseSz,
        crr = +1, cri = +1, clr = 0, cli = 0,
        ccr = +1, cci = -1, car = 0, cai = 0)

//      val inputSz         = irFrames / irSteps
      val inputSz         = (inputStepDur * sampleRate).toInt
      require (inputSz >= 1)
      val irFrames1       = irFrames // inputSz * irSteps
      require (irFrames1 >= 1)

      val minPhaseLog2    = Complex1FFT(in = minPhaseCepF, size = irMinPhaseSz) * irMinPhaseSz
      val minPhaseFFTOut  = minPhaseLog2.complex.exp
//      val audioSigMin0    = Real1FullIFFT(in = minPhaseFFTOut, size = irMinPhaseSz)
      val audioSigMin     = Real1FullIFFT(in = minPhaseFFTOut, size = irFrames1, padding = irMinPhaseSz - irFrames1)
//      val audioSigMin     = ResizeWindow(audioSigMin0, size = irMinPhaseSz, stop = -(irMinPhaseSz - irFrames1))
      val convSizeTime    = inputSz + irFrames1 - 1
      val convSizeFFT     = convSizeTime.nextPowerOfTwo

      println(s"irFrames $irFrames, irFrames1 $irFrames1, inputSz $inputSz, convSizeTime $convSizeTime, convSizeFFT $convSizeFFT")

      Length(audioSigMin).poll(0, "audioSigMin.length")

      val irFFT0          = Real1FFT(audioSigMin, size = irFrames1, padding = convSizeFFT - irFrames1)
      val audioIn         = AudioFileIn(file = fAudioIn, numChannels = 2)

      Length(audioIn).poll(0, "audioIn.length")

      val inputFFT        = Real1FFT(audioIn, size = inputSz, padding = convSizeFFT - inputSz)

      Length(inputFFT).poll(0, "inputFFT.length")

      val irFFTFade: GE = if (irSteps > 1) {
        val irFFT1          = BufferMemory(irFFT0, convSizeFFT)
        val irFFT2          = irFFT0.drop         (convSizeFFT)
        val irFFT1Rep       = RepeatWindow(irFFT1, size = convSizeFFT, num = irSteps)
        val irFFT2Rep       = RepeatWindow(irFFT2, size = convSizeFFT, num = irSteps)
        val irFFTFadeW0     = (ArithmSeq(start = 0, step = 1) % irSteps) / irSteps
        val irFFTFadeW      = RepeatWindow(irFFTFadeW0, num = convSizeFFT)
        val _irFFTFade      = irFFT1Rep * (-irFFTFadeW + (1.0: GE)) + irFFT2Rep * irFFTFadeW
        _irFFTFade
      } else {
        irFFT0
      }

      Length(irFFTFade).poll(0, "irFFTFade.length")

      val audioConvFFT    = irFFTFade.complex * inputFFT
      val audioConv       = Real1IFFT(audioConvFFT, size = convSizeTime, padding = convSizeFFT - convSizeTime)
      val audioConvLap    = OverlapAdd(audioConv, size = convSizeTime, step = inputSz)

      val specAudioOut = AudioFileSpec(numChannels = 4, sampleRate = sampleRate)
      AudioFileOut(in = audioConvLap, file = fAudioOut, spec = specAudioOut)
    }

    val cfg = stream.Control.Config()
//    cfg.blockSize  = fftSize
    cfg.useAsync   = false // for debugging
    val ctrl  = stream.Control(cfg)

    ctrl.run(gr)
    println("Running...")
    Await.result(ctrl.status, Duration.Inf)
  }
}
