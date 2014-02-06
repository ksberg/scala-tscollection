package bitzguild.scollection.function

import bitzguild.scollection.LeftSeq
import bitzguild.scollection.transform.LeftDoublesFunction

class SimpleMovingAverage extends LeftDoublesFunction {
  def apply(domain: LeftSeq[Double]) = domain.sum / domain.size
}