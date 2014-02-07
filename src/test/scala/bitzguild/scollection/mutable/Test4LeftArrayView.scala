package bitzguild.scollection.mutable


import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import bitzguild.scollection._

@RunWith(classOf[JUnitRunner])
class Test4LeftArrayView extends FlatSpec with ShouldMatchers {
  
  behavior of "LeftArrayView"
  
  it should "print when parent is empty" in {
    val s = new LeftArray[Int]()
    val v = s.view(5)
    val rep = v.toString
    rep should include ("Left")
    rep should include ("()")
  }
  
  it should "should facilitate endpoint overlap (index beyond bounds)" in {
    val series = new LeftArray[Int]()
    series += 1
    val v = series.view(5)
    for(i <- 0 to 4) assert(v(i) === 1)
  }
  
  it should "increment independent of view" in {
    val s = new LeftArray[Int](); s += 1
    val v = s.view(5); s += 2
    assert(v(0) != s(0))
    assert(v(0) === s(1))
  }
  
  it should "increment up to match parent" in {
    val s = new LeftArray[Int](); s += 1
    val v = s.view(5); s ++= Array(2,3,4)
    assert(v(0) != s(0))
    assert(v(0) === s(3))
    v.next; assert(v(0) === s(2))
    v.next; assert(v(0) === s(1))
    v.next; assert(v(0) === s(0))
    v.next; assert(v(0) === s(0))
  }
  
  it should "list elements in same order as parent" in {
    val s = new LeftArray[Int](); s ++= Array(1,2,3,4,5)
    val v = s.view(3)
    for(i <- 0 to 2) assert(s(i) === v(i))
  }
   
}