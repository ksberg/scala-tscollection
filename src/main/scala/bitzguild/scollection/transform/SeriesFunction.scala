package bitzguild.scollection.transform

import bitzguild.scollection.LeftSeq

trait LeftSeqFunction[A,LS >: LeftSeq[A]] {
  def apply(domain: LS): A
}
