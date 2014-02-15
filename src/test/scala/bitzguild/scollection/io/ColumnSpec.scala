package bitzguild.scollection.io


import org.scalatest.FlatSpec
import org.scalatest.matchers._
import org.scalatest.matchers.ShouldMatchers._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import bitzguild.scollection.mutable.LeftRing
import java.io.{PrintStream, ByteArrayOutputStream}

import spire.math._
import spire.implicits._
import spire.syntax.literals._

@RunWith(classOf[JUnitRunner])
class ColumnSpec extends FlatSpec with ShouldMatchers {

  behavior of "Column"

  it should "support spire extended number types" in {
    val size = 5
    val rR = new LeftRing[Rational](size)
    val rC = new LeftRing[Complex[Double]](size)

    rR += Rational(1,3)
    rC += Complex(0.0,1.0)

    val cR = new GenericColumn[Rational](rR,"Rational",10,new AlignRight)
    val cC = new GenericColumn[Complex[Double]](rC,"Complex",20, new AlignRight)

    val headerR = cR.renderName(true)
    headerR.size should be (10)
    headerR should include ("Rational")

    cR.renderName(false).size should be <  headerR.size

    val valueR = cR.renderValue(0,true)
    valueR.size should be (10)
    valueR should include ("1/3")

    cR.renderValue(0,false).size should be < valueR.size

    val headerC = cC.renderName(true)
    headerC.size should be (20)
    headerC should include ("Complex")

    val valueC = cC.renderValue(0,true)
    valueC.size should be (20)
    valueC should include ("0.0")
    valueC should include ("1.0i")

  }

}
