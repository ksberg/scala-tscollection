package bitzguild.scollection.mutable

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import bitzguild.scollection.RightSeries
import bitzguild.scollection.mutable._

@RunWith(classOf[JUnitRunner])
class Test4RightArray extends FlatSpec with ShouldMatchers {
  
  behavior of "RightArray"
  
  it should "be empty on create" in {
    val series = new RightArray[Int]()
    assert(series.size === 0)
  }
  
  it should "increment size on add" in {
    val series = new RightArray[Int]()
    for(i <- 1 to 10) {
      series += i
      assert(series.size === i)
    }
  }
  
  it should "add new elements at highest index" in {
    val initval = 1
    val testval = 13
    val series = new RightArray[Int]()
    series += initval
    series += testval
    assert(series(0) == initval)
    assert(series(series.size-1) === testval)
  }
  
  it should "list elements in order added" in {
    val ring = new RightArray[Int]()
    val arr = Array(1,2,3,4,5)
    ring ++= arr
    assert(ring.toArray === arr)
  }
    
}