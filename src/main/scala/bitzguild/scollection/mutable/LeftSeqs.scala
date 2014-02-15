package bitzguild.scollection.mutable

import scala.collection.generic.CanBuildFrom
import scala.collection.{immutable, GenSeq}
import scala.collection.mutable.ArrayBuffer
import bitzguild.scollection.{LeftView, MutableLeftSeq}

/**
 * Created by ksvenberg on 2/14/14.
 */
object LeftSeries {

}


// ------------------------------------------------------------------------------------
// LeftSeq Implementations
// ------------------------------------------------------------------------------------


/**
 * Common behavior and data between different LeftView implementations
 *
 * @param parent source data
 * @param lookback supported history
 * @param marker relative store in parent
 * @tparam A element type
 */
abstract class LeftInnerView[A](parent: MutableLeftSeq[A], lookback: Int, marker : Int) extends LeftView[A] {
  protected var csize = marker
  def another = parent.another
  def length = lookback
  def view(lookback: Int) = parent.view(lookback)
  def firstView(lookback: Int) = parent.firstView(lookback)
  override def toString = if(parent.size == 0) "LeftView()" else super.toString

  def reverseMap[B, That](f: (A) => B)(implicit bf: CanBuildFrom[GenSeq[A], B, That]): That = reverse.map(f)
  def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[GenSeq[A], B, That]): That = toList.updated(index,elem)
  def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[GenSeq[A], B, That]) = toList.+:(elem) //= toArray.+:(elem) _
  def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[GenSeq[A], B, That]) = toArray.:+ _
  def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[GenSeq[A], B, That]) = this.toArray.padTo _
  def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[GenSeq[A], B, That]): That = {
    toArray.patch(from,patch,replaced)
    asInstanceOf[That]
  }
}



/**
 * Common behavior for specialized LeftSeq implementations.
 * Implementation of these methods are required for concrete classes of IndexedSeq. The compile type
 * behavior differs for SBT, Eclipse, and IntelliJ; even after tweaking Idea settings to match SBT.
 * These methods produce copies of the original container.
 *
 * @tparam A element type
 */
abstract class LeftOuterContainer[A] extends collection.immutable.IndexedSeq[A] {
  override def reverseMap[B, That](f: (A) => B)(implicit bf: CanBuildFrom[immutable.IndexedSeq[A], B, That]) = reverse.map(f).asInstanceOf[That]
  override def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[immutable.IndexedSeq[A], B, That]) = {
    reverse.reverse.patch(from,patch,replaced)
    asInstanceOf[That]
  }
  def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[immutable.IndexedSeq[A], B, That]) = toList.updated _
  def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[immutable.IndexedSeq[A], B, That]) = toArray[A].+: _
  def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[immutable.IndexedSeq[A], B, That]) = toArray[A].:+ _
  def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[immutable.IndexedSeq[A], B, That]) = this.toArray.padTo _
}




/**
 * Fixed-size recycling storage buffer. Grows only by appending elements. Index zero represents
 * last element added and positive indices from zero access earlier elements.
 *
 * @param capacity fixed capacity
 * @tparam A element type
 */
class LeftRing[A](val capacity: Int = 5) extends LeftOuterContainer[A] with MutableLeftSeq[A] {

  class LeftRingView[A](parent: LeftRing[A], lookback: Int, cursor: Int) extends LeftInnerView[A](parent,lookback,cursor) {
    def data = parent.data
    def apply(i: Int) = data((csize + i) % data.size)
    def next = if (hasNext) { csize = csize-1; this } else this
    def hasNext = (csize != parent.cursor)
  }

  protected val cmax = (Int.MaxValue / capacity) * capacity - capacity
  protected var idx = cmax
  protected def cursorToLeft(i: Int)	= if (i == 0) cmax else i - 1
  protected def assignAndShift(elem: A) = { idx = cursorToLeft(idx); data(idx % capacity) = elem }
  protected def cursor = idx

  val data = new ArrayBuffer[A]()
  def length = data.length
  def apply(i: Int) = data((idx + i) % data.size)
  def +=(elem: A): this.type = {
    if (size < capacity) for (i <- 1 to capacity) data += elem
    else assignAndShift(elem)
    this
  }
  def ++=(col: Traversable[A]) : this.type = { col.foreach(e => this += e); this }
  def another = new LeftRing[A](capacity)
  def view(lookback: Int) = new LeftRingView(this,lookback,cursor)
  def firstView(lookback: Int) = new LeftRingView(this,lookback,cmax)
}





/**
 * Variable size array. Grows only by appending elements. Index zero represents
 * last element added and positive indices from zero access earlier elements.
 *
 * @tparam A element type
 */
class LeftArray[A]() extends LeftOuterContainer[A] with MutableLeftSeq[A] {

  class LeftArrayView[A](parent: LeftArray[A], val offset: Int, lookback: Int, capturesize : Int) extends LeftInnerView[A](parent,lookback,capturesize) {
    def apply(index: Int) = parent(Math.max(0,Math.min(parent.size-1,parent.size - csize + index)))
    def hasNext = (csize != parent.size)
    def next = if (hasNext) { csize = csize+1; this } else this
  }

  val arrdata = new collection.mutable.ArrayBuffer[A]()
  def length = arrdata.length
  def apply(i: Int) = arrdata((size - i - 1) % size)
  def +=(elem: A): this.type = {
    arrdata += elem
    this
  }
  def ++=(col: Traversable[A]) : this.type = { col.foreach(e => this += e); this }
  def another = new LeftArray[A]()
  def view(lookback: Int) = new LeftArrayView(this,0,lookback,arrdata.size)
  def firstView(lookback: Int) = new LeftArrayView(this,0,lookback,0)
}


