case class Reindeer(name: String, speed: Int, runTime: Int, restTime: Int)

def parseReindeer(string: String): Reindeer = {
  string.split(" ").toList match {
    case List(name, _, _, speed, _, _, runTime, _, _, _, _, _, _, restTime, _) =>
      Reindeer(name, speed.toInt, runTime.toInt, restTime.toInt)
  }
}

def moveReindeer(time: Int, r: Reindeer): List[Int] = {
  def move(time: Int, r: Reindeer, running: Boolean): List[Int] = {
    if (running) {
      if (time <= r.runTime)
        (1 to time).map(_ => r.speed).toList // 1, 1, 1, 1
      else if (time <= r.runTime + r.restTime)
        (1 to r.runTime).map(_ => r.speed).toList ++ (1 to (time - r.runTime)).map(_ => 0) // 1, 1, 0, 0
      else (1 to r.runTime).map(_ => r.speed).toList ++ move(time - r.runTime, r, running = false)
    } else {
      if (time <= r.restTime)
        (1 to time).map(_ => 0).toList // 0, 0, 0
      else if (time <= r.restTime + r.runTime)
        ((1 to r.restTime).map(_ => 0) ++ (1 to (time - r.restTime)).map(_ => r.speed)).toList // 0, 0, 1, 1
      else (1 to r.restTime).map(_ => 0).toList ++ move(time - r.restTime, r, running = true)
    }
  }
  move(time, r, running = true)
}

val test = "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\nDancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
val input = "Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds.\nBlitzen can fly 13 km/s for 4 seconds, but then must rest for 49 seconds.\nRudolph can fly 20 km/s for 7 seconds, but then must rest for 132 seconds.\nCupid can fly 12 km/s for 4 seconds, but then must rest for 43 seconds.\nDonner can fly 9 km/s for 5 seconds, but then must rest for 38 seconds.\nDasher can fly 10 km/s for 4 seconds, but then must rest for 37 seconds.\nComet can fly 3 km/s for 37 seconds, but then must rest for 76 seconds.\nPrancer can fly 9 km/s for 12 seconds, but then must rest for 97 seconds.\nDancer can fly 37 km/s for 1 seconds, but then must rest for 36 seconds."
val reindeers = input.split('\n').map(parseReindeer)
val reindeersRacetracks = reindeers
  .map(moveReindeer(2503, _))
  .map(_.scanLeft(0)(_ + _))
//1 2 3 4
var secondMaxes = collection.mutable.Map[Int, Int]()
for (second <- reindeersRacetracks.head.indices) {
  var maxInSecond = 0
  //List(0, 2, 2), List(0, 1, 2)
  for (track <- reindeersRacetracks) {
    if (track(second) > maxInSecond) maxInSecond = track(second)
  }
  secondMaxes(second) = maxInSecond
}
reindeersRacetracks
  .map(track => track
    .zipWithIndex
    .tail
    .map { case (result, index) => if (secondMaxes(index) == result) 1 else 0 }
    .sum
  )
  .max