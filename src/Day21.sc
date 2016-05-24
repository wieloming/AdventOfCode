case class Thing(cost: Int, damage: Int, armor: Int)
case class Player(name: String, var hitPoints: Int, initialAttack: Int, initialDefence: Int, stuff: List[Thing]) {
  def isAlive = hitPoints > 0

  def damage = initialAttack + stuff.map(_.damage).sum

  def defence = initialDefence + stuff.map(_.armor).sum

  override def toString =
    "hp: " + hitPoints + ", d: " + damage + ", def: " + defence

  def attack(enemy: Player): Player = {
    val attack = damage - enemy.defence
    val realAttack = if (attack > 1) attack else 1
    Player(enemy.name, enemy.hitPoints - realAttack, enemy.initialAttack, enemy.initialDefence, enemy.stuff)
  }
  def withStuff(newStuff: List[Thing]): Player = {
    Player(name, hitPoints, initialAttack, initialDefence, newStuff)
  }
}

def getEquipment = {
  val noThing = Thing(0, 0, 0)
  def parseData(s: String) = s
    .split('\n').toList
    .map(_.split("  ").toList.map(_.trim).filterNot(_.isEmpty))
    .map { case List(n, c, d, a) => Thing(c.toInt, d.toInt, a.toInt) }
  val weaponsData = "Dagger        8     4       0\nShortsword   10     5       0\nWarhammer    25     6       0\nLongsword    40     7       0\nGreataxe     74     8       0"
  val armorData = "Leather      13     0       1\nChainmail    31     0       2\nSplintmail   53     0       3\nBandedmail   75     0       4\nPlatemail   102     0       5"
  val ringData = "Damage +1    25     1       0\nDamage +2    50     2       0\nDamage +3   100     3       0\nDefense +1   20     0       1\nDefense +2   40     0       2\nDefense +3   80     0       3"
  (parseData(weaponsData), noThing :: parseData(armorData), noThing :: noThing :: parseData(ringData))
}
val (weapons, armors, rings) = getEquipment
def scenario(player1: Player, player2: Player): Boolean = {
  var p1 = player1
  var p2 = player2
  while (p1.isAlive && p2.isAlive) {
    p2 = p1.attack(p2)
    if (p2.isAlive) p1 = p2.attack(p1)
  }
  if (p1.isAlive && !p2.isAlive) true
  else false
}

def getEquipmentCombinations(weapons: List[Thing], armors: List[Thing], rings: List[Thing]): List[List[Thing]] = {
  val ringsCombination = rings.permutations.map(_.sliding(2)).flatten.toList.distinct
  for {
    weapon <- weapons
    armor <- armors
    rings <- ringsCombination
  } yield weapon :: armor :: rings
}
val gamer = Player("player", 100, 0, 0, List())
val boss = Player("boss", 109, 8, 2, List())
val heroes = getEquipmentCombinations(weapons,armors, rings).map(gamer.withStuff)
heroes.filterNot(scenario(_, boss)).map(_.stuff.map(_.cost).sum).max
