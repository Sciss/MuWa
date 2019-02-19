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

import de.sciss.fscape.{GE, Graph, graph, stream}
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.AudioFileSpec

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object PhaseAnalysis {
  def main(args: Array[String]): Unit = {
    val opt = Config.parse(args, name = "PhaseAnalysis")
    opt.fold(sys.exit(1)) { implicit config =>
      println(s"PhaseAnalysis ${Main.fullVersion}")
      val t0 = System.currentTimeMillis()
      val fut = run()
      Await.result(fut, Duration.Inf)
      val t1 = System.currentTimeMillis()
      println("Ok.")
      println(s"Took ${(t1 - t0)/1000} seconds.")
      sys.exit()
    }
  }

  def any2stringadd: Any = ()

  def run()(implicit config: Config): Future[Unit] = {
    import config._
    import graph._

    val gr = Graph {
      val videoIn0  = VideoFileIn(file = fVideoIn, numChannels = 3)
      val videoIn   = if (videoSkip == 0) videoIn0 else videoIn0.drop(videoSkip * (width * height))

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
//      val conv      = conjA.complex * fftB
      val conv      = fftB.complex * conjA
      val convMagR  = conv .complex.mag.max(1.0e-06).reciprocal
      val convBuf   = BufferMemory(conv, size = frameSize)
      val elemNorm  = convBuf * RepeatWindow(convMagR)
      val iFFT0     = Real2FullIFFT(in = elemNorm, rows = fftSize, columns = fftSize)
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

      tempImageOut.foreach { temp =>
        val phaseDiffI = (phaseDiff * 2).clip2(1.0) * 0.5 + (0.5: GE)
        val left      = ResizeWindow(phaseDiffI, size = fftSize, stop = fftSize)
        val right     = ResizeWindow(BufferDisk(crop).drop(frameSize), size = fftSize, start = -fftSize)
        val composite = left + right
        val specImageOut = ImageFile.Spec(width = fftSize * 2, height = fftSize, numChannels = 1,
          fileType = ImageFile.Type.JPG, sampleFormat = ImageFile.SampleFormat.Int8)
        ImageFileSeqOut(in = composite, template = temp, spec = specImageOut, indices = ArithmSeq(1, length = numFrames))
      }

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
//      val audioSigMin     = Real1FullIFFT(in = minPhaseFFTOut, size = irFrames1, padding = irMinPhaseSz - irFrames1)
      val audioSigMin     = {
        // XXX TODO IFFT - padding is broken?
        val i = Real1FullIFFT(in = minPhaseFFTOut, size = irMinPhaseSz)
        ResizeWindow(i, size = irMinPhaseSz, stop = -(irMinPhaseSz - irFrames1))
      }
//      val audioSigMin     = ResizeWindow(audioSigMin0, size = irMinPhaseSz, stop = -(irMinPhaseSz - irFrames1))
      val convSizeTime    = inputSz + irFrames1 - 1
      val convSizeFFT     = convSizeTime.nextPowerOfTwo

      println(s"irFrames $irFrames, irFrames1 $irFrames1, inputSz $inputSz, convSizeTime $convSizeTime, convSizeFFT $convSizeFFT")

//      Length(audioSigMin).poll(0, "audioSigMin.length")

      val irFFT0          = Real1FFT(audioSigMin, size = irFrames1, padding = convSizeFFT - irFrames1)
      val audioIn         = AudioFileIn(file = fAudioIn, numChannels = 2)

      Length(audioIn out 0).poll(0, "audioIn.length")

      val inputFFT        = Real1FFT(audioIn, size = inputSz, padding = convSizeFFT - inputSz)

      Length(inputFFT out 0).poll(0, "inputFFT.length")

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

//      Length(irFFTFade).poll(0, "irFFTFade.length")

      val audioConvFFT    = irFFTFade.complex * inputFFT
//      val audioConv       = Real1IFFT(audioConvFFT, size = convSizeTime, padding = convSizeFFT - convSizeTime)
      val audioConv       = {
        // XXX TODO IFFT - padding is broken?
        val i = Real1IFFT(audioConvFFT, size = convSizeFFT)
        ResizeWindow(i, size = convSizeFFT, stop = -(convSizeFFT - convSizeTime))
      }

      Length(audioConv out 0).poll(0, "audioConv.length")

      val audioConvLap    = OverlapAdd(audioConv, size = convSizeTime, step = inputSz)
      val hpf             = HPF(audioConvLap, freqN = 75.0 / sampleRate) * gain

      val specAudioOut = AudioFileSpec(numChannels = 4, sampleRate = sampleRate)
      AudioFileOut(in = hpf, file = fAudioOut, spec = specAudioOut)
    }

    val cfg = stream.Control.Config()
//    cfg.blockSize  = fftSize
    cfg.useAsync   = false // for debugging
    val ctrl  = stream.Control(cfg)

    ctrl.run(gr)
    println("Running...")
    ctrl.status
//    sys.exit()
  }
}
