package bitzguild.scollection.transform

import bitzguild.scollection.LeftSeq

abstract class LeftDoublesFunction extends LeftSeqFunction[Double, LeftSeq[Double]] {
  def apply(domain: LeftSeq[Double]): Double 
}