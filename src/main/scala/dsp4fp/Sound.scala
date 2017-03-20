package dsp4fp

import javax.sound.sampled._

import dsp4fp.sound.{AudioSignal, Morse}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}

object Sound extends App {

  case object Done

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


  AudioSignal.fromFunction(Math.sin).loop().toAudio(44000).play(true)

  AudioSignal.random().take(50).loop().toAudio(AudioSignal.pcmu8bit16khz.getSampleRate.toInt * 2).play(true)
  
  AudioSignal.random().take(100).loop().toAudio(AudioSignal.pcmu8bit16khz.getSampleRate.toInt * 2).play(true)

  AudioSignal.random().take(50).decaying(0.99).toAudio(16000).play(true)
  AudioSignal.random().take(100).decaying(0.99).toAudio(16000).play(true)
  AudioSignal.random().take(10).decaying(0.999).toAudio(16000).play(true)

  Morse.text("SOS").allToAudio().play(true)

  AudioSignal.simultaneous(
    AudioSignal.fromFunction(Math.sin).loop(),
    AudioSignal.random().take(50).decaying(0.99),
    AudioSignal.random().take(120).decaying(0.99),
    AudioSignal.random().take(10).decaying(0.999)
  ).toAudio(32000 * 2).play(true)


}
