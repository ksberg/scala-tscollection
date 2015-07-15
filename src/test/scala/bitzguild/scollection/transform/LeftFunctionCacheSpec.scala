package bitzguild.scollection.transform

import org.scalatest.FlatSpec
import org.scalatest.matchers._
import org.scalatest.matchers.ShouldMatchers._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import bitzguild.scollection.mutable.{LeftArray, LeftRing}
import java.io.{PrintStream, ByteArrayOutputStream}

import spire.math._
import spire.implicits._
import spire.syntax.literals._
import bitzguild.scollection.LeftSeq
import bitzguild.scollection.function.SimpleMovingAverage

@RunWith(classOf[JUnitRunner])
class LeftFunctionCacheSpec extends FlatSpec with ShouldMatchers {

  behavior of "LeftFunctionCache"

  it should "allow configuration during empty state" in {
    val d1 = new LeftArray[Double]()
    val c1 = new LeftFunctionCache(d1,new SimpleMovingAverage,3)

    c1.size should be (0)
    c1.toString should include ("LeftFunctionCache")

    val v1 = c1.view(3)
    v1.size should be (3)
    v1.toString should include ("LeftCacheView")
  }

  it should "match storage type of domain" in {
    val d1 = new LeftArray[Double](); d1 ++= Array(1.0,2.0,3.0,4.0,5.0)
    val c1 = new LeftFunctionCache[Double](d1,new SimpleMovingAverage,3)

    c1.cache.getClass should be theSameInstanceAs d1.getClass

    val d2 = new LeftRing[Double](6); d2 ++= Array(1.0,2.0,3.0,4.0,5.0)
    val c2 = new LeftFunctionCache[Double](d2, new SimpleMovingAverage, 3)

    c2.cache.getClass should be theSameInstanceAs d2.getClass
  }

  it should "grow with domain for array storage (Array)" in {
    val a = new LeftArray[Double](); a ++= Array(1.0,2.0,3.0,4.0,5.0)
    val c = new LeftFunctionCache[Double](a,new SimpleMovingAverage,3)

    a.size should be (5)
    c.size should be (5)
    a(4) should be (1.0)
    c(4) should be (1.0)

    a += 6.0
    a.size should be (c.size)
    a(5) should be (1.0)  // 1st in array moves right
    c(5) should be (1.0)  // cache should match
  }

  it should "remain same size as domain (Ring)" in {
    val a = new LeftRing[Double](4); a ++= Array(1.0,2.0,3.0,4.0,5.0)
    val c = new LeftFunctionCache[Double](a,new SimpleMovingAverage,3)

    a.size should be (4)
    c.size should be (4)

    a += 6.0
    a.size should be (c.size)
    a.size should be (4)

  }


  it should "cache same results with either Ring or Array storage" in {
    val rdomain = new LeftRing[Double](4);
    val adomain = new LeftArray[Double]();

    val rcache = new LeftFunctionCache[Double](rdomain,new SimpleMovingAverage,3)
    val acache = new LeftFunctionCache[Double](adomain,new SimpleMovingAverage,3)

    for(i <- 0 to 10) {
      rdomain += i
      adomain += i
      val minsize = Math.min(rdomain.size,adomain.size)

      val rtake = rcache.take(minsize)
      val atake = acache.take(minsize)

      rtake should equal (atake)
    }
  }

  it should "return latest view (Array)" in {
    val a = new LeftArray[Double](); a ++= Array(1.0,2.0,3.0,4.0,5.0)
    val c = new LeftFunctionCache[Double](a,new SimpleMovingAverage,3)

    val v = c.view(3)

    v.size should be (3)
    v(0) should be (c(0))
    v(1) should be (c(1))
    v(2) should be (c(2))
  }

  it should "return latest view (Ring)" in {
    val a = new LeftRing[Double](4); a ++= Array(1.0,2.0,3.0,4.0,5.0)
    val c = new LeftFunctionCache[Double](a,new SimpleMovingAverage,3)

    val v = c.view(3)

    v.size should be (3)
    v(0) should be (c(0))
    v(1) should be (c(1))
    v(2) should be (c(2))
  }

  it should "indirectly increment separate from view" in {
    val a = new LeftArray[Double](); a ++= Array(1.0,2.0,3.0,4.0,5.0)
    val c = new LeftFunctionCache[Double](a,new SimpleMovingAverage,3)

    val v = c.view(3)

    v(0) should be (c(0))

    a += 6.0
    v(0) should be (c(1))

    a += 7.0
    v(0) should be (c(2))
  }

  it should "allow view to increment up to current state" in {
    val a = new LeftArray[Double](); a ++= Array(1.0,2.0,3.0,4.0,5.0)
    val c = new LeftFunctionCache[Double](a,new SimpleMovingAverage,3)

    val v = c.view(3)
    a ++= Array(6.0,7.0)

    v(0) should be (c(2))
    v.next
    v.next
    v(0) should be (c(0))
  }
}
