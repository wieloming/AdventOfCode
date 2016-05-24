case class Buff(healing: Int, hitting: Int, manaRestoring: Int, magicArmor: Int, var turnsLeft: Int, manaNeeded: Int, var isActive: Boolean) {
  def affect(player: Player, enemy: Player): (Player, Player) = {
    if (turnsLeft > 0) {
      turnsLeft = turnsLeft - 1
      (player.heal(healing).restoreMana(manaRestoring).magicArmor(magicArmor), enemy.hit(hitting))
    } else (player.magicArmor(0), enemy)
  }
}
case class Player(name: String, hitPoints: Int, mana: Int, armor: Int, damage: Int) {
  def isAlive = hitPoints > 0
  def hit(amount: Int) = Player(name, hitPoints - amount, mana, armor, damage)
  def restoreMana(amount: Int) = Player(name, hitPoints, mana + amount, armor, damage)
  def heal(amount: Int) = Player(name, hitPoints + amount, mana, armor, damage)
  def magicArmor(amount: Int) = Player(name, hitPoints, mana, armor + amount, damage)

  def attack(amount: Int) = {

  }
  //  override def toString = "hp: " + hitPoints + ", d: " + damage + ", def: " + defence
}


//def attack(spell: Buff, enemy: Player): (Player, Player) = {
//  val manaLeft = mana - spell.manaNeeded
//  if (isAlive && enemy.isAlive && manaLeft >= 0) {
//    (p1, p2) = spell.affect(this, enemy)
//    val p1 = tick()
//    val p2 = enemy.tick()
//  }
//}



var buffs: List[Buff] = List()
var activeBuffs: List[Buff] = List()

def scenario(player1: Player, player2: Player, buffsList: List[Buff]): Boolean = {
  def tick(buffs: List[Buff], p1: Player, p2: Player): (Player, Player) = {
    buffs.foldLeft((p1, p2))((players, buff) => buff.affect(players._1, players._2))
  }
  def activateBuff(buff: Buff, activeBuffs: List[Buff]): List[Buff] = {
    if(!buff.isActive){
      buff.isActive = true
      buff::activeBuffs
    }else activeBuffs
  }

  var (p1, p2) = (player1, player2)
  var buffs = buffsList

  while (p1.isAlive && p2.isAlive) {
    //P1 attack
    activeBuffs = activateBuff(buffs.head, activeBuffs)
    buffs = buffs.tail
    val (p1, p2) = tick(activeBuffs, p1, p2)
    if (p2.isAlive) p1 = p1.attack(p2.damage)
  }
  if (p1.isAlive && !p2.isAlive) true
  else false
}

