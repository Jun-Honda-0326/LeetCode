object Easy {

  def main(args: Array[String]): Unit = {
    val accounts: Array[Array[Int]] = Array(Array(1, 2, 3), Array(4, 5, 6))
    val nums: List[Int] = List(1, 2, 3, 4, 5)
    val nums2: List[Int] = List(1, 2, 3, 1, 1, 3)
    val candies: List[Int] = List(2, 3, 5, 6, 1, 2)
    val int = 3
    val address: String = "255.255.100.1"
    val jewels: String = "aA"
    val stones:String = "aAAbbb"


    println(maxmunWealth(accounts))
    println(runningSum(nums))
    println(defangIPadder(address))
    println(kidsWithCandies(candies, int))
    println(shuffle(candies, int))
    println(numIdenticalPairs(nums2))
    println(numJewelsInStones(jewels, stones))
    println(smallerNumbersThanCurrent(nums2))
    println(restoreString("codeleet", Array(4,5,6,7,0,2,1,3)))
    println(subtractProductAndSum(234))

  }


  def maxmunWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(v => v.sum).max
  }

  //累積和問題
  def runningSum(nums: List[Int]): List[Int] = {
    nums.scanLeft(0)(_ + _).slice(1, nums.size + 1)
  }

  def defangIPadder(address: String): String = {
    address.replace(".", "[.]")
  }

  def kidsWithCandies(candies: List[Int], extraCandies: Int): List[Boolean] = {
    candies.map(v =>
      if (candies.max <= v + extraCandies) {
        true
      } else {
        false
      }
    )
  }

  def shuffle(nums: List[Int], n: Int): List[Int] = {
    val nums1 = nums.splitAt(n)._1.zipWithIndex
    val nums2 = nums.splitAt(n)._2.zipWithIndex
    val newNums = nums1 ++ nums2
    newNums.sortBy(v => v._2).map(x => x._1)
  }

  def numIdenticalPairs(nums: List[Int]): Int = {
    val nums1 = nums.zipWithIndex
    val nums2 = nums.zipWithIndex.drop(1)
    val numsTup = for {
      num1 <- nums1
      num2 <- nums2
    } yield (num1, num2)
    numsTup.filter(v => v._1._1 == v._2._1 & v._1._2 < v._2._2).length
  }

   def numJewelsInStones(jewels: String, stones: String): Int = {
      stones.filter(v => jewels.contains(v)).length
   }

   def smallerNumbersThanCurrent(nums: List[Int]): List[Int] = {
     for (i <- nums) yield {
       nums.filter(v => v < i).length
     }
    }

     def restoreString(s: String, indices: Array[Int]): String = {
        indices.zip(s).sortBy(_._1).map(_._2).mkString
     }

     def subtractProductAndSum(n: Int): Int = {
       val list = n.toString.map(_.toString.toInt).toSeq
       (list.reduce(_ * _) - list.reduce(_ + _)).toInt
    }


}
