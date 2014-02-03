package bitzguild.scollection.mutable

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import bitzguild.scollection.LeftSeries
import bitzguild.scollection.mutable._

@RunWith(classOf[JUnitRunner])
class Test4LeftRing extends FlatSpec with ShouldMatchers {
  
  behavior of "LeftRing"
  
  it should "be empty on create" in {
    val ldef = new LeftRing[Int]()
    val lcap = new LeftRing[Int](5)
    assert(ldef.size === 0)
    assert(lcap.size === 0)
  }
  
  it should "be full on add" in {
    val capacity = 5
    val testval = 13
    val lcap = new LeftRing[Int](capacity)
    lcap += testval
    assert(lcap.size === capacity)
    for(i <- 0 until capacity) assert(lcap(i) === testval)
  }
  
  it should "add new elements at zero" in {
    val capacity = 5
    val initval = 1
    val testval = 13
    val lcap = new LeftRing[Int](capacity)
    lcap += initval
    lcap += testval
    assert(lcap.size === capacity)
    assert(lcap.head === testval)
    for(i <- 1 until capacity) assert(lcap(i) === initval)
  }
  
  it should "never exceed capacity" in {
    val capacity = 5
    val initval = 1
    val testval = 13
    val lcap = new LeftRing[Int](capacity)
    for(i <- 1 to capacity*2) lcap += testval
    assert(lcap.size === capacity)
  }
  
  it should "list elements in reverse of order added" in {
    val capacity = 5
    val l = new LeftRing[Int](capacity)
    val arr = Array(1,2,3,4,5)
    l ++= arr
    val larr = l.toArray
    assert(larr.reverse === arr)
  }
  
  it should "drop oldest elements greater than capacity" in {
    val capacity = 5
    val l = new LeftRing[Int](capacity)
    val arr = Array(1,2,3,4,5,6)
    l ++= arr
    val larr = l.toArray
    assert(larr.reverse === arr.tail)
  }
  
  it should "be assignable and manipulated from common Series type" in {
    val capacity = 5
    var series : LeftSeries[Int] = null
    series = new LeftRing[Int](capacity)
    series += 1
    series ++= Array(1,2,3,4,5,6)
    assert(series.size === capacity)
    assert(series.head === 6)
    assert(series.last === 2)
  }
  

}