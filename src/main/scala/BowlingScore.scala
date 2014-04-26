object BowlingScore {

  def apply(frameString: String) = {
    score(frameString.toList, 10, 0)
  }

  def score(frameString: List[Char], remainingFrames: Int, acc: Int): Int = {
    (frameString, remainingFrames) match {
      case (_, 0)                    => acc
      case ('X' :: h2 :: h3 :: t, _) => score(h2::h3::t, remainingFrames - 1, acc + 10 + pinsBowled(h2, h3))
      case (h1::'/'::h3::t, _)       => score(h3::t, remainingFrames - 1, acc + 10 + pinsBowled(h3))
      case (h1::h2::t, _)            => score(t, remainingFrames - 1, acc + pinsBowled(h1, h2))
    }
  }

  def pinsBowled(C: Char): Int = {
    C match {
      case '-' => 0
      case 'X' => 10
      case x   => x.asDigit
    }
  }

  def pinsBowled(C: Char, D: Char): Int = (C, D) match {
    case (_, '/') => 10
    case (x, y)   => pinsBowled(x) + pinsBowled(y)
  }
}
