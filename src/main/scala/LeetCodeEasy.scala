object Easy {

  def main(args: Array[String]): Unit = {
    val accounts: Array[Array[Int]] = Array(Array(1, 2, 3), Array(4, 5, 6))
    val nums: List[Int] = List(1,2,3,4,5)
    println(maxmunWealth(accounts))
    println(runningSum(nums))
  }

  def maxmunWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(v => v.sum).max
  }

  def runningSum(nums: List[Int]): List[Int] = {
    nums.scanLeft(0)(_ + _).slice(1, nums.size + 1)
  }

}
