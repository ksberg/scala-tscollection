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
    val dv = domain.firstView(lookback)
    cacheFunction.init(dv) 
    dv
  }
  def length = domain.length
  def apply(index: Int) = {
    while(dview.hasNext) cache += cacheFunction(dview.next)
    cache(index)
  }
  def firstView(len: Int) = cache.firstView(len)
  def view(len: Int) = cache.view(len)
  def another = cache.another
}

class LeftFunctionCombo[A: Numeric](a: LeftSeq[A], b: LeftSeq[A], f: (A,A) => A) extends LeftSeq[A] {
  def length = Math.min(a.length,b.length)
  def apply(index: Int) = f(a(index),b(index))
  def another = a.another
  def view(len: Int) = null
  def firstView(len: Int) = null
}

