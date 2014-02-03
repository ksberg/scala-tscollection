package bitzguild.scollection.mutable


import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import bitzguild.scollection.LeftSeries
import bitzguild.scollection.mutable._

@RunWith(classOf[JUnitRunner])
class Test4LeftArray extends FlatSpec with ShouldMatchers {
  
  behavior of "LeftArray"
  
  it should "be empty on create" in {
    val series = new LeftArray[Int]()
    assert(series.size === 0)
  }
  
  it should "increment size on add" in {
    val series = new LeftArray[Int]()
    for(i <- 1 to 10) {
      series += i
      assert(series.size === i)
    }
  }
  
  it should "add new elements at zero index" in {
    val initval = 1
    val testval = 13
    val series = new LeftArray[Int]()
    series += initval
    series += testval
    assert(series(0) == testval)
    assert(series(series.size-1) === initval)
  }
  
  it should "list elements in reverse order added" in {
    val ring = new LeftArray[Int]()
    val arr = Array(1,2,3,4,5)
    ring ++= arr
    assert(ring.toArray.reverse === arr)
  }

}