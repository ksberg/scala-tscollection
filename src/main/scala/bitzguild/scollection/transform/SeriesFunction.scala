package bitzguild.scollection.transform

import bitzguild.scollection.{LeftSeq,LeftView}

trait LeftSeqFunction[A,LS >: LeftSeq[A]] {
  def init(domain: LS) : Unit
  def apply(domain: LS): A
}

abstract class LeftFunctionCache[A: Numeric](
    domain : LeftSeq[A],
    cacheFunction: LeftSeqFunction[A, LeftSeq[A]],
    lookback : Int
    ) extends LeftSeq[A] {

  val cache = domain.another
  val dview = domain.view(lookback)	// val implies that view must be mutable in order to increment
  def length = domain.length
  def initD = {
      cacheFunction.init(dview) 
  }
  def apply(index: Int) = {
    while(dview.hasNext) {
      cache += cacheFunction(dview.next)
    }
    cache(index)
  }
  
  // prevent ops
  def +=(elem: A): LeftSeq[A]
  def ++=(col: Traversable[A]) : LeftSeq[A]

  def view(len: Int) = cache.view(len)
  def another = cache.another
  
}
