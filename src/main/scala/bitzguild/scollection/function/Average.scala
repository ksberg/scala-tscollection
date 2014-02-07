package bitzguild.scollection.function

import bitzguild.scollection.LeftSeq
import bitzguild.scollection.transform.LeftDoublesFunction

class SimpleMovingAverage extends LeftDoublesFunction {
  def init(domain: LeftSeq[Double]) = {}
  def apply(domain: LeftSeq[Double]) = domain.sum / domain.size
}

class ExponentialMovingAverage(val length: Int) extends LeftDoublesFunction {
  val alpha : Double = 2.0 / (1 + length)
  def init(domain: LeftSeq[Double]) = {}
  def apply(domain: LeftSeq[Double]) = domain.sum / domain.size
}