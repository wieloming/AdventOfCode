def count(string: String) = {
  var previous = ' '
  def addDashBeforeNewChar(ch: Char): String = {
    if (previous == ch) ch.toString
    else {
      previous = ch
      "-" + ch
    }
  }
  string
    .view
    .map(addDashBeforeNewChar)
    .mkString
    .split("-")
    .tail
    .map(num => num.length.toString + num.head)
    .mkString
}

var input = "1321131112"
for(times <- 0 until 50){
  input = count(input)
}
input.length
