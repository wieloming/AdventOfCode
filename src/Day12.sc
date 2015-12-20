import scala.io._
import scala.util._

val source = Source
  .fromFile("C:/Users/Lukasz/Desktop/JaVa/Scala/AdventOfCode/data/json.txt").getLines.toList
  .mkString

val regex = """(-|)[0-9]+""".r

def parse(input: Any): Int = input match {
  case list: List[Any] => list.map {
    case number: Double => number.toInt
    case list: List[Any] => parse(list)
    case map: Map[Any, Any] => parse(map)
    case _ => 0
  }.sum
  case map: Map[Any, Any] =>
    if (map.values.exists(_ == "red")) 0
    else map.values.map(x=> parse(List(x))).sum
}

val part1 = regex.findAllIn(source).toList.map(_.toInt).sum
val part2 = parse(parsing.json.JSON.parseFull(source).get)