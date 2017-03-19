package dsp4fp

import java.io.ByteArrayInputStream
import javax.sound.sampled._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.Random

object Sound extends App {

  case object Done

  val pcmu8bit16khz = new AudioFormat(16000.0f, 8, 1, true, true)

  implicit class AudioInputStreamWithPlay(val ais: AudioInputStream) extends AnyVal {
    def play(blocking: Boolean = false): Future[Done.type] = {
      val p = Promise[Done.type]()

      val clip = AudioSystem.getClip()
      clip.open(ais)
      clip.addLineListener((event: LineEvent) => {
        event.getType match {
          case LineEvent.Type.STOP =>
            p.success(Done)
            clip.close()
          case _ =>
        }
      })
      clip.start()
      if (blocking) Await.result(p.future, Duration.Inf)
      p.future
    }
  }

  object AudioSignal {

    // XXX: does this need a more descriptive name?
    // This really only works for functions that return from -1 to 1
    def fromFunction(func: Double => Double): AudioSignal = {
      AudioSignal(
        Stream.iterate(0)(_ + 1).map { incr =>
          (func(incr.toDouble / (2 * Math.PI)) * Byte.MaxValue).toByte
        }
      )
    }

    def random() = AudioSignal(Stream.continually(Random.nextInt().toByte))
  }


  case class AudioSignal(data: Stream[Byte]) extends AnyVal {

    protected def zeroPadded = data ++ Stream.continually(0: Byte)
    def loop() = AudioSignal(Stream.continually(data).flatten)

    def |+|(other: AudioSignal) = {
      AudioSignal(
        zeroPadded.zip(other.zeroPadded).map { case (a, b) => (a + b).toByte }
      )
    }

    def delay(frames: Int) = AudioSignal(Stream.fill[Byte](frames)(0) ++ data)

    def take(frames: Int) = AudioSignal(data.take(frames))

    def toAudio(samples: Long, format: AudioFormat = pcmu8bit16khz) = {
      new AudioInputStream(
        new ByteArrayInputStream(zeroPadded.take(samples.toInt).toArray), format, AudioSystem.NOT_SPECIFIED
      )
    }
  }

  AudioSignal.fromFunction(Math.sin).loop().toAudio(44000).play(true)
  AudioSignal.random().take(50).loop().toAudio(pcmu8bit16khz.getSampleRate.toInt * 2).play(true)
  AudioSignal.random().take(100).loop().toAudio(pcmu8bit16khz.getSampleRate.toInt * 2).play(true)


}
