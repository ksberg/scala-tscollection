package bitzguild.scollection.io

import org.scalatest.FlatSpec
import org.scalatest.matchers._
import org.scalatest.matchers.ShouldMatchers._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import bitzguild.scollection.mutable.LeftRing
import java.io.{PrintStream, ByteArrayOutputStream}

@RunWith(classOf[JUnitRunner])
class TableSpec extends FlatSpec with ShouldMatchers {

  behavior of "Table"

  it should "be empty and valid on create" in {
    val t = new Table()
    val os = new ByteArrayOutputStream
    val ps = new PrintStream(os)

    t.top(5,ps)
    val s = os.toString
    s should startWith ("Empty")


  }

  it should "support basic column types" in {
    val size = 5
    val rI = new LeftRing[Int](size)
    val rL = new LeftRing[Long](size)
    val rD = new LeftRing[Double](size)
    val rC = new LeftRing[Char](size)
    val rB = new LeftRing[Boolean](size)

    val t = new Table()
    t.addInts(rI,"Ints")
    t.addLongs(rL,"Longs")
    t.addDoubles(rD,"Doubles")
    t.addChars(rC,"Chars")
    t.addBooleans(rB,"Booleans")

    val os = new ByteArrayOutputStream
    val ps = new PrintStream(os)

    rI += 1
    rL += 1
    rD += 1
    rC += 'a'
    rB += true
    t.top(3,ps)

    val s = os.toString

    s should include ("Ints")
    s should include ("Longs")
    s should include ("Doubles")
    s should include ("Chars")
    s should include ("Booleans")
  }

  it should "support multiple sized columns" in {
    val rI = new LeftRing[Int](10); rI += 1
    val rL = new LeftRing[Long](4); rL += 1
    val rD = new LeftRing[Double](15); rD += 1

    val t = new Table()
    t.addInts(rI,"Ints")
    t.addLongs(rL,"Longs")
    t.addDoubles(rD,"Doubles")

    t.size should be (4)
  }

  it should "clip rows to max size" in {
    val rI = new LeftRing[Int](10); rI += 1
    val rL = new LeftRing[Long](4); rL += 1
    val rD = new LeftRing[Double](15); rD += 1

    val t = new Table()
    t.addInts(rI,"Ints")
    t.addLongs(rL,"Longs")
    t.addDoubles(rD,"Doubles")

    val os = new ByteArrayOutputStream
    val ps = new PrintStream(os)

    t.top(200,ps)
    val s = os.toString
    val splits = s.split("\n")

    splits.size should be (5)
  }

  it should "have total row width related to columns" in {
    val rI = new LeftRing[Int](10); rI += 1
    val rL = new LeftRing[Long](4); rL += 1
    val rD = new LeftRing[Double](15); rD += 1

    val t = new Table()
    t.addInts(rI,"Ints",10)
    t.addLongs(rL,"Longs",10)
    t.addDoubles(rD,"Doubles",10)

    val os = new ByteArrayOutputStream
    val ps = new PrintStream(os)
    t.header(ps," ",true)

    val s = os.toString
    val sline = s.split("\n")(0)
    sline.size should be (10+1+10+1+10)
  }

  it should "render comma separated values" in {
    val rI = new LeftRing[Int](10); rI += 1
    val t = new Table()
    t.addInts(rI,"Ints",10)
    t.addInts(rI,"Ints",10)
    t.addInts(rI,"Ints",10)
    t.addInts(rI,"Ints",10)

    val os = new ByteArrayOutputStream
    val ps = new PrintStream(os)

    t.csv(ps)

    val s = os.toString
    val splits = s.split("\n")
    val header = splits(0)
    val row = splits(1)

    header should include (",")
    header.filter(_ == ',').size should be (3)

    row should include (",")
    row.filter(_ == ',').size should be (3)
  }

  it should "render tab separated values" in {
    val rI = new LeftRing[Int](10); rI += 1
    val t = new Table()
    t.addInts(rI,"Ints",10)
    t.addInts(rI,"Ints",10)
    t.addInts(rI,"Ints",10)
    t.addInts(rI,"Ints",10)

    val os = new ByteArrayOutputStream
    val ps = new PrintStream(os)

    t.tsv(ps)

    val s = os.toString
    val splits = s.split("\n")
    val header = splits(0)
    val row = splits(1)

    header should include ("\t")
    val headerColumns = header.split("\t")
    headerColumns.size should be (3+1)   // TABS + CR

    row should include ("\t")
    val rowColumns = row.split("\t")
    rowColumns.size should be (3+1)     // TABS + CR
  }
}
