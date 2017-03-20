package dsp4fp.sound

object Morse {

  val ditLength = 1200

  val dit = AudioSignal.fromFunction(Math.sin).loop().take(ditLength)
  val ▄ = dit
  val dah = AudioSignal.fromFunction(Math.sin).loop().take(ditLength * 3)
  val ▄▄▄ = dah
  val elementGap = AudioSignal.silence.loop().take(ditLength)
  var letterGrap = AudioSignal.silence.loop().take(ditLength * 3)
  val wordGap = AudioSignal.silence.loop().take(ditLength * 7)

  def morseChar(signals: AudioSignal*) = AudioSignal.sequence(signals.map(_.andThen(elementGap)):_*)

  val letters = Map(
    'O' -> morseChar(▄▄▄, ▄▄▄, ▄▄▄),
    'S' -> morseChar(▄, ▄, ▄)
  )

  def text(s: String): AudioSignal = {
    AudioSignal.sequence(
      s.map { char =>
        letters.getOrElse(char, letterGrap).andThen(letterGrap)
      }: _*
    )
  }
}
