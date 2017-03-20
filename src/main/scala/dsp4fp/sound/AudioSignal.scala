package dsp4fp.sound

import java.io.ByteArrayInputStream
import javax.sound.sampled.{AudioFormat, AudioInputStream, AudioSystem}

import scala.util.Random

object AudioSignal {

  val pcmu8bit16khz = new AudioFormat(16000.0f, 8, 1, true, true)

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

  val empty = AudioSignal(Stream.empty)

  val silence = AudioSignal(Stream.continually(0: Byte))

  def simultaneous(signals: AudioSignal*) = signals.fold(empty)(_ |+| _)

  def sequence(signals: AudioSignal*) = signals.fold(empty)(_ ++ _)

}


case class AudioSignal(data: Stream[Byte]) extends AnyVal {

  protected def zeroPadded = data ++ Stream.continually(0: Byte)
  def loop() = AudioSignal(Stream.continually(data).flatten)

  def |+|(other: AudioSignal) = {
    AudioSignal(
      zeroPadded.zip(other.zeroPadded).map { case (a, b) => (a + b).toByte }
    )
  }

  def scale(d: Double): AudioSignal = {
    AudioSignal(
      data.map(b => (b * d).toByte)
    )
  }

  def delay(frames: Int) = AudioSignal(Stream.fill[Byte](frames)(0) ++ data)

  def decaying(factor: Double): AudioSignal = {
    AudioSignal(
      Stream.iterate(factor)(_ * factor).flatMap { dFactor =>
        data.map(b => (b * dFactor).toByte)
      }
    )
  }

  def take(frames: Int) = AudioSignal(data.take(frames))

  def toAudio(samples: Long, format: AudioFormat = AudioSignal.pcmu8bit16khz) = {
    new AudioInputStream(
      new ByteArrayInputStream(zeroPadded.take(samples.toInt).toArray), format, AudioSystem.NOT_SPECIFIED
    )
  }

  def allToAudio(format: AudioFormat = AudioSignal.pcmu8bit16khz) = {
    new AudioInputStream(
      new ByteArrayInputStream(data.toArray), format, AudioSystem.NOT_SPECIFIED
    )
  }

  def ++(o: AudioSignal) = andThen(o)

  def andThen(other: AudioSignal) = AudioSignal(this.data ++ other.data)
}
