object Day01 {
  def problem1(input: List[String]): Int = {
    getTotalCaloriesPerElf(input).max
  }

  def problem2(input: List[String]): Int = {
    getTotalCaloriesPerElf(input).sorted.reverse.take(3).sum
  }

  def getTotalCaloriesPerElf(input: List[String]): List[Int] = {
    input.foldLeft(List.empty[List[String]]) {
      case (acc, "") => List.empty :: acc
      case (List(), item) => List(List(item))
      case (items :: acc, item) => (items :+ item) :: acc
    }.reverse
      .map(_.map(_.toInt))
      .map(_.sum)
  }

  def main(args: Array[String]): Unit = {
    val input = Utils.read("input01")
    println(problem1(input))
    println(problem2(input))
  }
}