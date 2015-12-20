
val testInput = "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8\nCinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
val input = "Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5\nPeanutButter: capacity -1, durability 3, flavor 0, texture 0, calories 1\nFrosting: capacity 0, durability -1, flavor 4, texture 0, calories 6\nSugar: capacity -1, durability 0, flavor 0, texture 2, calories 8"
case class Ingredient(capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int) {
  def multipleWithoutCalories(amount: Int) = Seq(capacity * amount, durability * amount, flavor * amount, texture * amount)
  def multipleWithCalories(amount: Int) = Seq(capacity * amount, durability * amount, flavor * amount, texture * amount, calories * amount)
}
def toIngredient(string: String): Ingredient = {
  (string + ",").split(" ").toList match {
    case List(_, _, cap, _, dur, _, fla, _, tex, _, cal) =>
      Ingredient(cap.init.toInt, dur.init.toInt, fla.init.toInt, tex.init.toInt, cal.init.toInt)
  }
}
val ingredients = input
  .split('\n')
  .map(toIngredient)
  .toList
val ingredientsCombinations: Iterator[List[(Int, Ingredient)]] =
  ingredients
    .flatMap(_ => (0 to 100).toList) // 0..100  0..100
    .combinations(ingredients.size) // 1,2,3,4  2,1,3,6  5,4,31,5
    .filter(_.sum == 100) // 0,0,0,100  0,0,1,99
    .flatMap(_.permutations) // 0,0,0,100  0,0,100,0
    .map(_.zip(ingredients)) // (0,I1 0,I2 0,I3 100,I4) (0,I1 0,I2...

def sum(input: List[Seq[Int]]) = input.transpose.map(_.sum.max(0))
def sumWithCaloriesLimit(input: List[Seq[Int]]) = input.transpose.map(_.sum.max(0))

//val part1 = ingredientsCombinations
//  .map { ingComb =>
//    val ingredientTimesAmount = ingComb.map { case (amount, i) => i.multipleWithoutCalories(amount) }
//    sum(ingredientTimesAmount).product
//  }
//  .max

val part2 = ingredientsCombinations
  .map { ingComb =>
    val ingredientTimesAmount = ingComb.map { case (amount, i) => i.multipleWithCalories(amount) }
    sumWithCaloriesLimit(ingredientTimesAmount)
  }
  .filter(_.last == 500)
  .map(_.init.product)
  .max

