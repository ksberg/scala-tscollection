package bitzguild.scollection.transform

import bitzguild.scollection.LeftSeq

trait LeftSeqFunction[A,LS >: LeftSeq[A]] {
  def init(domain: LS) : Unit
  def apply(domain: LS): A
}
