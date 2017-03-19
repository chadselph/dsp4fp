package dsp4fp

import java.io.ByteArrayInputStream
import javax.sound.sampled._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.Random

object Sound extends App {

  case object Done

  val pcmu16khz = new AudioFormat(16000.0f, 8, 1, true, true)

  implicit class SeqIntsToAudio(val data: Seq[Int]) extends AnyVal {
    def toAudio(format: AudioFormat = pcmu16khz) = {
      new AudioInputStream(new ByteArrayInputStream(data.toArray.map(_.toByte)), format, AudioSystem.NOT_SPECIFIED
      )
    }
  }

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

  implicit class AudioDoubleFunc(val m: Double => Double) extends AnyVal {
    def toAudio(samples: Int = 44100, format: AudioFormat = pcmu16khz) = {
      val period = (0 to 255).map(n =>
        (m((n / 128.0) * (Math.PI / 2)) * 128).toInt
      )
      SeqIntsToAudio(Stream.continually(period).flatten.take(samples)).toAudio(format)
    }
  }

  def repeat[A](seq: Seq[A], length: Int): Seq[A] = Stream.continually(seq).flatten.take(length)
  def randomInts(size: Int) = List.iterate(0, size)(_ => Random.nextInt())

  val randomSeed = List.iterate(0, 50)(_ => Random.nextInt())
  val twoSecClip = repeat(randomSeed, pcmu16khz.getSampleRate.toInt * 2)
  val lower = repeat(randomInts(100), pcmu16khz.getSampleRate.toInt * 2)
  twoSecClip.toAudio().play(true)
  lower.toAudio().play(true)


}

