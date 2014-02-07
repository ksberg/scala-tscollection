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

 object SeriesDefaults {
   var capacity = 3
}

// ------------------------------------------------------------------------------------
// These work as intended ...
// ------------------------------------------------------------------------------------

trait LeftSeq[A] extends IndexedSeq[A] {
  def view(lookback: Int) : LeftView[A]
  def another : LeftSeq[A]
}

trait LeftView[A] extends LeftSeq[A] {
  def next : LeftView[A]
  def hasNext : Boolean
}

trait MutableLeftSeq[A] extends LeftSeq[A] {
  def +=(elem: A): LeftSeq[A]
  def ++=(col: Traversable[A]) : LeftSeq[A]
}





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
 * Variable size array. Grows only by appending elements. Index zero represents
 * last element added and positive indices from zero access earlier elements.
 */
class LeftArray[A]() extends collection.immutable.IndexedSeq[A] with MutableLeftSeq[A] {
  
	class LeftArrayView[A](val parent: LeftArray[A], val offset: Int, lookback: Int, val capturesize : Int) extends LeftView[A] {
	  def length = lookback
	  def apply(index: Int) = parent(Math.max(0,Math.min(parent.size-1,parent.size - capturesize + index)))
	  def next = if(capturesize == parent.size) this else new LeftArrayView(parent,offset,lookback,capturesize+1)
	  def hasNext = (capturesize != parent.size)
	  
	  def view(lookback: Int) = parent.view(lookback)
	  def another = parent.another
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

/**
 * Fixed-size recycling storage buffer. Grows only by appending elements. Index zero represents
 * last element added and positive indices from zero access earlier elements.
 */
class LeftRing[A](val capacity: Int = 5) extends collection.immutable.IndexedSeq[A] with MutableLeftSeq[A] {
  
	class LeftRingView[A](val parent: LeftRing[A], lookback: Int, val cursor: Int) extends collection.immutable.IndexedSeq[A] with LeftView[A] {
	  protected lazy val cmax =(Int.MaxValue / parent.capacity) * parent.capacity - parent.capacity
	  def data = parent.data
	  def length = lookback
	  def apply(i: Int) = data((cursor + i) % data.size)
	  def next = if (cursor == parent.cursor) this else new LeftRingView[A](parent,lookback, parent.cursorToLeft(cursor))
	  def hasNext = (cursor != parent.cursor)
	  def view(lookback: Int) = parent.view(lookback)
	  def another = parent.another

//    def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[immutable.IndexedSeq[A], B, That]): That = ???
//    def reverseMap[B, That](f: (A) => B)(implicit bf: CanBuildFrom[immutable.IndexedSeq[A], B, That]): That = ???
//    def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[immutable.IndexedSeq[A], B, That]): That = ???
//    def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[immutable.IndexedSeq[A], B, That]): That = ???
//    def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[immutable.IndexedSeq[A], B, That]): That = ???
//    def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[immutable.IndexedSeq[A], B, That]): That = ???
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





class Guard[A](indata: IndexedSeq[A]) extends IndexedSeq[A] {
  def data = indata 	
  def length = indata.length
  def apply(i: Int) = indata(Math.max(0,Math.min(i,indata.size-1)))

//  def reverseMap[B, That](f: (A) => B)(implicit bf: CanBuildFrom[GenSeq[A], B, That]) = ???
//  def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[GenSeq[A], B, That]) = ???
//  def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[GenSeq[A], B, That]) = ???
//  def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[GenSeq[A], B, That]) = ???
//  def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[GenSeq[A], B, That]) = ???
//  def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[GenSeq[A], B, That]) = ???
}





