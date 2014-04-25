
object BowlingScore {

  //have to create an extractor to get at the proper value
  // I have to extract stuff that might be a digit or -
  object BowlingNumber {
    def unapply(value: Char): Option[Int] = {
      if (value == '-') {
        Some(0)
      } else if (value.isDigit) {
        Some(value.asDigit)
      } else {
        None
      }
    }
  }


  def apply(frames: String) = {

    def calculateStrike(fl: List[Char]): Int = {
      val pair = (fl.head, fl.tail.head)
      val rolls = pair match {
        case (_, '/') => {
          10
        }
        case (BowlingNumber(one), BowlingNumber(two)) => {
          one + two
        }
        case (one, two) => {
          scorePins(one) + scorePins(two)
        }
      }

      10 + rolls
    }

    def calculateSpare(fl: List[Char]): Int = {
      scorePins(fl.head)
    }

    def scorePins(pins: Char): Int = {
      pins match {
        case '-' => 0
        case 'X' => 10
        case x => x.asDigit
      }
    }

    def calculateScore(frameIndex: Int, score: Int, fl: List[Char], previous: Char): Int = {
      if (fl.isEmpty || frameIndex == 10) {
        //If we're all done, or we hit the 10th frame
        score
      } else {
        val pins = fl.head

        pins match {
          case BowlingNumber(x) => {
            //It's a number lets get two items
            val nextRoll = fl.tail.head
            val frameScore = nextRoll match {
              case BowlingNumber(y) => {
                y + x
              }
              case '/' => {
                calculateSpare(fl.tail.tail) + (10 - x) + x
              }
            }
            calculateScore(frameIndex + 1, score + frameScore, fl.tail.tail, nextRoll)
          }
          case 'X' => {
            calculateScore(frameIndex + 1, score + calculateStrike(fl.tail), fl.tail, pins)
          }
        }
      }
    }

    val pinsList = frames.toList
    calculateScore(0, 0, pinsList, '-')
  }
}
