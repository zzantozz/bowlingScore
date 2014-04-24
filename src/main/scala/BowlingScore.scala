
object BowlingScore {
  def apply(frames:String) = {
    frames.toList.foldLeft(0)((a,b) => {
      if(b == '-') {
        a
      } else {
        a + b.asDigit
      }
    })
  }
}
