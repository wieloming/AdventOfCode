
def increaseWithTurn(string: String): String = {
  var turnNext = true
  def nextChar(char: Char) = if (char == 'z') 'a' else (char to 'z').tail.head
  string
    .reverse
    .map { ch =>
      if (turnNext) {
        if (ch != 'z') turnNext = false
        nextChar(ch)
      } else ch
    }
    .reverse
}

//must include one increasing straight of at least three letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count.
def increasing3(string: String) = string
  .sliding(3)
  .toList
  .map(_.toList)
  .exists { case x :: xs => x :: xs == (x to xs.last).toList }

//not contain the letters i, o, or l, as these letters can be mistaken for other characters and are therefore confusing.
def notContains(string: String) = List('i', 'o', 'l').forall(!string.contains(_))

//must contain at least two different, non-overlapping pairs of letters, like aa, bb, or zz.
def twoDifferentPairs(string: String) = {
  def count(string:List[Char]):Int = string match {
    case x::y::xs if x==y =>  1 + count(xs)
    case x::y::xs => count(y::xs)
    case _ => 0
  }
  count(string.toList) >= 2
}

var result = increaseWithTurn("hxbxxyzz")
while(!increasing3(result) || !notContains(result) || !twoDifferentPairs(result)){
  result = increaseWithTurn(result)
}
result







