package bitzguild.scollection.mutable

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import bitzguild.scollection.RightSeries
import bitzguild.scollection.mutable._

@RunWith(classOf[JUnitRunner])
class Test4RightRing extends FlatSpec with ShouldMatchers {
  
  behavior of "RightRing"
  
  it should "be empty on create" in {
    val ringdef = new RightRing[Int]()
    val ringcap = new RightRing[Int](5)
    assert(ringdef.size === 0)
    assert(ringcap.size === 0)
  }
  
  it should "be full on add" in {
    val capacity = 5
    val testval = 13
    val ringcap = new RightRing[Int](capacity)
    ringcap += testval
    assert(ringcap.size === capacity)
    for(i <- 0 until capacity) assert(ringcap(i) === testval)
  }
  
  it should "add new elements at highest index" in {
    val capacity = 5
    val initval = 1
    val testval = 13
    val ringcap = new RightRing[Int](capacity)
    ringcap += initval
    ringcap += testval
    assert(ringcap.size === capacity)
    assert(ringcap.last === testval)
    for(i <- 0 until capacity-1) assert(ringcap(i) === initval)
  }
  
  it should "never exceed capacity" in {
    val capacity = 5
    val initval = 1
    val testval = 13
    val ringcap = new RightRing[Int](capacity)
    for(i <- 1 to capacity*2) ringcap += testval
    assert(ringcap.size === capacity)
  }
  
  it should "list elements in order added" in {
    val capacity = 5
    val ring = new RightRing[Int](capacity)
    val arr = Array(1,2,3,4,5)
    ring ++= arr
    assert(ring.toArray === arr)
  }
  
  it should "drop oldest elements greater than capacity" in {
    val capacity = 5
    val ring = new RightRing[Int](capacity)
    val arr = Array(1,2,3,4,5,6)
    ring ++= arr
    assert(ring.toArray === arr.tail)
  }
  
  it should "be assignable and manipulated from common Series type" in {
    val capacity = 5
    var series : RightSeries[Int] = null
    series = new RightRing[Int](capacity)
    series += 1
    series ++= Array(1,2,3,4,5,6)
    assert(series.size === capacity)
    assert(series.head === 2)
    assert(series.last === 6)
  }

}