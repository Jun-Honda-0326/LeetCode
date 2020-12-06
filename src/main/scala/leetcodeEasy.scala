object Easy{

  def main(args:Array[String]):Unit = {

    val accounts: Array[Array[Int]] = Array(Array(1,2,3), Array(4,5,6))
    println(maxmunWealth(accounts))

  }

  def maxmunWealth(accounts: Array[Array[Int]]):Int = {
    accounts.map(v => v.sum).max
  }
}
