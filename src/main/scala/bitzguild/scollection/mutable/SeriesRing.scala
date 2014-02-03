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

package bitzguild.scollection.mutable

import bitzguild.scollection._
import scala.collection.mutable.ArrayBuffer

/**
 * WIP:
 * 
 * def flip : LeftRingBuffer[A]
 * def latest(n: Int) : RightRingBuffer[A] 
 * ... slice view of last added, especially useful for look-back function calculations
 */
trait RightRingBuffer[A] extends SeriesRing[A] with RightSeriesStore[A] {}
trait LeftRingBuffer[A]  extends SeriesRing[A] with LeftSeriesStore[A] {}

 /**
  * Abstract Ring Buffer base class
  *
  * @param capacity buffer capacity
  * @tparam A buffer element type
  */
abstract class BaseRingBuffer[A](val capacity: Int) 
				extends collection.immutable.IndexedSeq[A] 
				with SeriesRing[A] {

  protected lazy val cmax = (Int.MaxValue / capacity) * capacity - capacity
  protected val arrdata = new ArrayBuffer[A]()
  protected def data = arrdata
  protected var idx : Int = cursorInit
  protected def assignAndShift(elem: A) : Unit
  
  def cursor = idx
  def cursorInit : Int
  def cursorMax = cmax
  def index = idx % capacity
  def another : BaseRingBuffer[A]
  
  def +=(elem: A): this.type = {
    if (size < capacity) for (i <- 1 to capacity) arrdata += elem
    else assignAndShift(elem)
    this
  }
  def ++=(col: Traversable[A]) : this.type = { col.foreach(e => this += e); this }
}



 /**
  * Fixed-length buffer that grows from the left (i.e. zero index gets new element).
  * Zero length until 1st element is added, then buffer is allocated to capacity. 
  *
  * @param capacity buffer capacity
  * @tparam A buffer element type
  */
class LeftRing[A](capacity: Int = SeriesDefaults.capacity) 
	extends BaseRingBuffer[A](capacity) with LeftRingBuffer[A] {

	class LeftRingRef[A](d: IndexedSeq[A], i: Int)  extends BaseRingBufferRef[A](d,i)  
													with LeftSeriesRef[A] {
	  def next = { idx = cursorToLeft(idx); LeftRingRef.this }
	  def view(offset: Int = 0) = new LeftRingRef(d, idx + Math.abs(offset))
	}
  
  def cursorInit = cursorMax
  def view(offset: Int = 0) = new LeftRingRef(arrdata, idx + Math.abs(offset))
  def another = new LeftRing[A](capacity)
  protected def assignAndShift(elem: A) = { idx = cursorToLeft(idx); data(index) = elem } 
}



 /**
  * Fixed-length buffer that grows from the right (i.e. high index gets new element).
  * Zero length until 1st element is added, then buffer is allocated to capacity. 
  *
  * @param capacity buffer capacity
  * @tparam A buffer element type
  */
class RightRing[A](capacity: Int = SeriesDefaults.capacity) 
	extends BaseRingBuffer[A](capacity) with RightRingBuffer[A] {
  
	class RightRingRef[A](d: IndexedSeq[A], i: Int) extends BaseRingBufferRef[A](d,i) 
													with RightSeriesRef[A] {
	  def next = { idx = cursorToRight(idx); this }
	  def view(offset: Int = 0) = new RightRingRef(d, idx - Math.abs(offset))
	}
  
  def cursorInit = 0
  def view(offset: Int = 0) = new RightRingRef(arrdata,idx - Math.abs(offset))
  def another = new RightRing[A](capacity)
  protected def assignAndShift(elem: A) = { data(index) = elem; idx = cursorToRight(idx) }
}