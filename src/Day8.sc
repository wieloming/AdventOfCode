// calculate how long is string
//"\x27" is 6 characters of code, but the string itself
// contains just one - an apostrophe ('), escaped using
// hexadecimal notation.
import scala.io._

val source = Source
  .fromFile("C:/Users/Lukasz/Desktop/JaVa/Scala/AdventOfCode/data/input.txt").getLines.toList

val strings = source
  .map(_.length)
  .sum

val inMemory = source
  .map(
    _.replace("\\\\", "o")
      .replace("\\\"", "o")
      .replace("\"", "")
      .replaceAll("\\\\x[0-9a-f]{2}", "o")
      .length)
  .sum

val encoded = source
  .map(
    _.replaceAll("\\\\x[0-9a-f]{2}", "ooooo")
      .replace("\\\"", "oooo")
      .replace("\\\\", "oooo")
      .length + 4
  ).sum

strings - inMemory
encoded - strings