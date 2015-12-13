import scala.io._

val strings = Source.fromFile("C:/Users/Lukasz/Desktop/JaVa/Scala/AdventOfCode/data/input.txt").getLines.toList

strings
  .map(s => (length(s), s.length, s))
  .foreach(println)


def length(string: String): Int = {
  string.replace("""\"""+"x", "").filterNot(_ == "\").length - 2
}