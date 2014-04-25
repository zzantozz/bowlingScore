object BowlingScore {
  def apply(frames: String) = {

    def calculateStrike(fl: List[Char]): Int = {
      //Assuming valid game
      val r1 = fl.head
      val r2 = fl.tail.head
      val rolls = if (r2 == '/') {
        //If the second roll is a spare, then the combination of the two is 10
        10
      } else {
        //Otherwise, we're going to count up that stuff, and it's either another strike or a number
        scorePins(r1) + scorePins(r2)
      }
      10 + rolls
    }

    def calculateSpare(fl: List[Char]): Int = {
      scorePins(fl.head)
    }

    def scorePins(pins: Char): Int = {
      if (pins == '-') {
        0
      } else if (pins == 'X') {
        10
      } else {
        pins.asDigit
      }
    }

    def calculateScore(frameIndex: Int, score: Int, fl: List[Char], previous: Char): Int = {
      if (fl.isEmpty || frameIndex == 10) {
        //If we're all done, or we hit the 10th frame
        score
      } else {
        val pins = fl.head
        //I have a number of pins hit
        // Could be - [1-9] / or X
        if (pins.isDigit || pins == '-') {
          //If we're starting as a number, we'll examine the two in a pair
          val nextPins = fl.tail.head
          val nextScore = if (nextPins == '-' || nextPins.isDigit) {
            scorePins(nextPins)
          } else {
            //It's a spare -- This is kind of icky
            calculateSpare(fl.tail.tail) + (10 - scorePins(pins))
          }

          //At this point, we've done two numbers, or a number and a spare, add one to the frame index
          calculateScore(frameIndex + 1, score + scorePins(pins) + nextScore, fl.tail.tail, pins)
        } else if (pins == 'X') {
          //Strike!
          calculateScore(frameIndex + 1, score + calculateStrike(fl.tail), fl.tail, pins)
        } else {
          //TODO: should not need this, need to better scope my crap
          0
        }
      }
    }

    val pinsList = frames.toList
    calculateScore(0, 0, pinsList, '-')
  }
}
