val maxHouseNumber = 800000
val target = 29000000 / 11

def getPresentsForHousesUpTo(maxHouseNumber: Int) = {
  val elfsWithPresents = (for {
    houseNum <- 1 to maxHouseNumber
    elf <- houseNum to maxHouseNumber by houseNum if elf / houseNum <= 50
  } yield (elf, houseNum))
    .groupBy { case (elf, houseNum) => elf }
    .mapValues(_.map(_._2).sum)
    .toList

  elfsWithPresents
    .sortBy { case (elf, presents) => elf }
    .map(_._2)
}

getPresentsForHousesUpTo(maxHouseNumber).indexWhere(_ >= target) + 1
