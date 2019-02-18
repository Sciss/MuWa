package de.sciss.muwa

import de.sciss.file._
import de.sciss.fscape.{GE, Graph, graph, stream}
import de.sciss.numbers.Implicits._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object PhaseAnalysis {
  case class Config(fIn: File, tempOut: File,
                    width: Int = 960, height: Int = 540, numFrames: Int = 10)

  def main(args: Array[String]): Unit = {
    val workLaptop  = file("/data/temp")
    val videoDir    = if (workLaptop.isDirectory) workLaptop else userHome / "Videos"
    val imageDir    = if (workLaptop.isDirectory) workLaptop else userHome / "Pictures"

    val default = Config(
      fIn     = videoDir / "bla3.h264",
      tempOut = imageDir / "bla-%d.jpg"
    )

    val p = new scopt.OptionParser[Config]("Read Video Test") {
      opt[File]('i', "input")
        .text("Input video file")
        .action { (f, c) => c.copy(fIn = f) }

      opt[File]('o', "output")
        .text("Output jpg template - use %d as frame number place holder")
        .action { (f, c) => c.copy(tempOut = f) }

      opt[Int]('w', "width")
        .text("Video/image width in pixels")
        .action { (v, c) => c.copy(width = v) }

      opt[Int]('h', "height")
        .text("Video/image height in pixels")
        .action { (v, c) => c.copy(height = v) }

      opt[Int]('n', "num-frames")
        .text("Number of frames to render")
        .action { (v, c) => c.copy(numFrames = v) }
    }
    p.parse(args, default).fold(sys.exit(1)) { implicit config =>
      run()
    }
  }

  def any2stringadd: Any = ()

  def run()(implicit config: Config): Unit = {
    import graph._
    import config._

    val gr = Graph {
      val videoIn   = VideoFileIn(file = fIn, numChannels = 3)
      val fftSize   = (math.min(width, height) / 2).nextPowerOfTwo
      val mono: GE = {
        val r = videoIn.out(0)
        val g = videoIn.out(1)
        val b = videoIn.out(2)
        r * 0.2126 + g * 0.7152 + b * 0.0722
      }
      val crop: GE          = {
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
      val windowed  = mkWindow(crop)
      val seqA      = BufferMemory(windowed, frameSize)
      val seqB      = crop.drop             (frameSize)
      val fftA      = Real2FullFFT(seqA, rows = fftSize, columns = fftSize)
      val fftB      = Real2FullFFT(seqB, rows = fftSize, columns = fftSize)

      val conjA     = fftA .complex.conj  // A is to be shift against B!
      val conv      = conjA.complex * fftB
      val convMagR  = conv .complex.mag.max(1.0e-06).reciprocal
      val convBuf   = BufferMemory(conv, size = frameSize)
      val elemNorm  = convBuf * RepeatWindow(convMagR)
      val iFFT0     = Real2FullIFFT(in = elemNorm, rows = fftSize, columns = fftSize)
      val iFFT      = iFFT0 / (fftSize * 2) + 0.5

//      val specOut = ImageFile.Spec(width = fftSize, height = fftSize, numChannels = 1,
//        fileType = ImageFile.Type.JPG, sampleFormat = ImageFile.SampleFormat.Int8)
//      ImageFileSeqOut(in = iFFT, template = tempOut, spec = specOut, indices = ArithmSeq(1, length = numFrames))

      val left      = ResizeWindow(iFFT, size = fftSize, stop = fftSize)
      val right     = ResizeWindow(BufferDisk(crop), size = fftSize, start = -fftSize)
      val composite = left + right

      val specOut = ImageFile.Spec(width = fftSize * 2, height = fftSize, numChannels = 1,
        fileType = ImageFile.Type.JPG, sampleFormat = ImageFile.SampleFormat.Int8)
      ImageFileSeqOut(in = composite, template = tempOut, spec = specOut, indices = ArithmSeq(1, length = numFrames))

//      val analyzeWinType    = GenWindow.Hann
//      val synthesizeWinType = GenWindow.Rectangle
//      val inputWinSize      = 16384 /* 4096 */
//      val templateWinSize   = 16384 /* 32768 */
//      val stepSize          = 1024 /* 16 */
//
//      val winSize       = math.max(inputWinSize, templateWinSize)
//      val winAnaIn      = GenWindow(size = inputWinSize   , shape = analyzeWinType)
//      val winAnaTemp    = GenWindow(size = templateWinSize, shape = analyzeWinType)
//      val winSynth      = GenWindow(size = inputWinSize   , shape = synthesizeWinType)
//
//      val winSizeH  = winSize >> 1
//      val radiusI   = math.max(1, math.min(winSizeH - 1, (radius * winSizeH + 0.5).toInt))
//
//      // println(s"fftSize = $fftSize; numFrames = $numFrames; stepSize = $stepSize; inputWinsize = $inputWinSize")
//
//      // val inputPadLen     = inputWinSize - stepSize
//      val inputPadLen     = inputWinSize/2
//      val templatePadLen  = inputPadLen // templateWinSize - stepSize
//      // we pad the input so the when we apply the window, we don't
//      // lose the first frames
//      val inputPad        = DC(0.0).take(inputPadLen   ) ++ input
//      val templatePad     = DC(0.0).take(templatePadLen) ++ template
//
//      val slideA    = Sliding(in = inputPad   , size = inputWinSize   , step = stepSize)
//      val slideB    = Sliding(in = templatePad, size = templateWinSize, step = stepSize)
//      val winA      = slideA * winAnaIn
//      val winB      = slideB * winAnaTemp
//      val winARes   = ResizeWindow(in = winA, size = inputWinSize   , start = 0, stop = fftSize - inputWinSize   )
//      val winBRes   = ResizeWindow(in = winB, size = templateWinSize, start = 0, stop = fftSize - templateWinSize)
//
//      val fftA      = Real2FullFFT(in = winARes, rows = fftSize, columns = fftSize)
//      val fftB      = Real2FullFFT(in = winBRes, rows = fftSize, columns = fftSize)
//
//      // cf. https://en.wikipedia.org/wiki/Phase_correlation
//      val conjA     = fftA .complex.conj  // A is to be shift against B!
//      val conv      = conjA.complex * fftB
//      val convMagR  = conv .complex.mag.max(1.0e-06).reciprocal
//      val convBuf   = BufferDisk(conv)    // XXX TODO -- measure max delay
//      val elemNorm  = convBuf * RepeatWindow(convMagR)
//      val iFFT0     = Real1FullIFFT(in = elemNorm, size = fftSize)
//      val iFFT      = iFFT0 / fftGain
//
//      val prod      = PeakCentroid1D(in = iFFT, size = fftSize, radius = radiusI)
//      val shiftX    = prod.translate
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
