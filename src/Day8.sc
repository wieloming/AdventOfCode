import scala.io._

val strings = Source.fromFile("C:/Users/Lukasz/Desktop/JaVa/Scala/AdventOfCode/data/input.txt").getLines.toList

strings
  .map(s => s.length - length(s))
  .sum

def length(string: String): Int = {
  val hexes = string.length - string.replace("""\"""+"""x""", "x").length
  string
    .replace("""\"""+"x", "")
    .replace("""\""", "")
    .length - 2 - hexes
}