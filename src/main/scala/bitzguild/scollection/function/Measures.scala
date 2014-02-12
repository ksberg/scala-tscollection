package bitzguild.scollection.function

import bitzguild.scollection.LeftSeq
import bitzguild.scollection.transform.LeftDoublesFunction
import bitzguild.scollection.transform._

/**
 * Highest value in the domain scoping length.
 */
class Highest extends LeftDoublesFunction {
  def init(domain: LeftSeq[Double]) = {}
  def apply(domain: LeftSeq[Double]) = domain.max
}

/**
 * Lowest value in the domain scoping length.
 */
class Lowest extends LeftDoublesFunction {
  def init(domain: LeftSeq[Double]) = {}
  def apply(domain: LeftSeq[Double]) = domain.min
}

class Momentum(length: Int) extends LeftDoublesFunction {
  val len = length -1
  def init(domain: LeftSeq[Double]) = {}
  def apply(domain: LeftSeq[Double]) = domain(0) - domain(len)
}


class EfficiencyRatio(val length: Int) extends LeftDoublesFunction {
  def init(domain: LeftSeq[Double]) = {}
  def apply(domain: LeftSeq[Double]) = {
    val signal = Math.abs(domain(0) - domain(length-1))
    // val noise = domain.zip(domain.tail).foldLeft(0.0)((s,t) => s + Math.abs(t._2 - t._1))
    var noise = 0.0
    for(i <- 0 until length-1) noise += Math.abs(domain(i)-domain(1))
    if (noise == 0.0) 1.0 else Math.abs(signal/noise)
  }
}

object Measures {

}