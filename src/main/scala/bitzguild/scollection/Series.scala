/* ***** BEGIN LICENSE BLOCK *****
 *
 * Copyright (c) 2001-2014, Kevin Sven Berg. All rights reserved.
 *
 * This package is part of the Bitzguild Distribution
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the <organization> nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ***** END LICENSE BLOCK ***** */

 package bitzguild.scollection

import scala.collection.{immutable, GenSeq, IndexedSeq}
import scala.collection.mutable.ArrayBuffer
import scala.collection.generic.CanBuildFrom

// ------------------------------------------------------------------------------------
// LeftSeq Interface
// ------------------------------------------------------------------------------------

trait LeftSeq[A] extends IndexedSeq[A] {
  def view(lookback: Int) : LeftView[A]
  def another : MutableLeftSeq[A]
}

trait LeftView[A] extends LeftSeq[A] {
  def next : LeftView[A]
  def hasNext : Boolean
}

trait MutableLeftSeq[A] extends LeftSeq[A] {
  def +=(elem: A): LeftSeq[A]
  def ++=(col: Traversable[A]) : LeftSeq[A]
}


// ------------------------------------------------------------------------------------
// LeftSeq Implementations
// ------------------------------------------------------------------------------------

// not one ...
class RView[A](val data: IndexedSeq[A], val offset: Int, len: Int) extends IndexedSeq[A] {
  def length = len
  def apply(index: Int) = data(offset + index)
}


/**
 * This is a immutable participant. Note that view() should behave differently
 * with sequences that are not assumed to grow. We want view to start at first
 * element so that next will iterate over the sequence of static elements.
 */
class LeftWrap[A](data: IndexedSeq[A]) extends collection.immutable.IndexedSeq[A] {
  def length = data.length
  def apply(i: Int) = data(data.size - i - 1 % data.size)
//  def view(lookback: Int) = new LAView(data,data.size-1,lookback,data.size)
}

/**
 * Common behavior and data between different left view implementations
 */
abstract class LeftInnerView[A](val parent: MutableLeftSeq[A], lookback: Int, val marker : Int) extends LeftView[A] {
  protected var csize = marker
  def another = parent.another
  def length = lookback
  def view(lookback: Int) = parent.view(lookback)
}


/**
 * Fixed-size recycling storage buffer. Grows only by appending elements. Index zero represents
 * last element added and positive indices from zero access earlier elements.
 */
class LeftRing[A](val capacity: Int = 5) extends collection.immutable.IndexedSeq[A] with MutableLeftSeq[A] {
  
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
  def view(lookback: Int) = new LeftRingView(this,lookback,cursor)
  def another = new LeftRing[A](capacity)
}




/**
 * Variable size array. Grows only by appending elements. Index zero represents
 * last element added and positive indices from zero access earlier elements.
 */
class LeftArray[A]() extends collection.immutable.IndexedSeq[A] with MutableLeftSeq[A] {
  
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
}


