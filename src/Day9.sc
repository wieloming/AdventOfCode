//find the closest way knowing distance between cities like
//London to Dublin = 464
//London to Belfast = 518
//Dublin to Belfast = 141

import scala.collection.breakOut
case class Connection(from: String, to: String, distance: Long)
val input = "Faerun to Tristram = 65\nFaerun to Tambi = 129\nFaerun to Norrath = 144\nFaerun to Snowdin = 71\nFaerun to Straylight = 137\nFaerun to AlphaCentauri = 3\nFaerun to Arbre = 149\nTristram to Tambi = 63\nTristram to Norrath = 4\nTristram to Snowdin = 105\nTristram to Straylight = 125\nTristram to AlphaCentauri = 55\nTristram to Arbre = 14\nTambi to Norrath = 68\nTambi to Snowdin = 52\nTambi to Straylight = 65\nTambi to AlphaCentauri = 22\nTambi to Arbre = 143\nNorrath to Snowdin = 8\nNorrath to Straylight = 23\nNorrath to AlphaCentauri = 136\nNorrath to Arbre = 115\nSnowdin to Straylight = 101\nSnowdin to AlphaCentauri = 84\nSnowdin to Arbre = 96\nStraylight to AlphaCentauri = 107\nStraylight to Arbre = 14\nAlphaCentauri to Arbre = 46"
val connections: List[Connection] = input
  .split('\n')
  .map(_
    .split(" ")
    .toList)
  .map { case c1 :: _ :: c2 :: _ :: d :: Nil => Connection(c1, c2, d.toLong) }(breakOut)
def ways(connections: List[Connection]): List[List[String]] = {
  connections
    .flatMap(c => List(c.from, c.to))
    .distinct
    .permutations
    .toList
}
def findDistance(from: String, to: String, connections: List[Connection]): Long = {
  connections
    .filter(c => (c.from == from && c.to == to) || (c.to == from && c.from == to))
    .head
    .distance
}
ways(connections)
  .map(_
    .sliding(2)
    .toList
    .map { case x :: y :: Nil => findDistance(x, y, connections) }
    .sum)
  .max
