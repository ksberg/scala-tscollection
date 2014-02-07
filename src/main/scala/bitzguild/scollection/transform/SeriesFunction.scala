package bitzguild.scollection.transform

import bitzguild.scollection.{LeftSeq,LeftView}

trait LeftSeqFunction[A,LS >: LeftSeq[A]] {
  def init(domain: LS) : Unit
  def apply(domain: LS): A
}

class LeftFunctionCache[A: Numeric](
    domain : LeftSeq[A],
    cacheFunction: LeftSeqFunction[A, LeftSeq[A]],
    lookback : Int
    ) extends LeftSeq[A] {

  val cache = domain.another
  val dview = {
    val dv = domain.view(lookback)
    cacheFunction.init(dv) 
    dv
  }
  def length = domain.length
  def apply(index: Int) = {
    while(dview.hasNext) cache += cacheFunction(dview.next)
    cache(index)
  }
  def view(len: Int) = cache.view(len)
  def another = cache.another
}
