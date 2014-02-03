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


trait RightSeriesArray[A] extends SeriesArray[A] with RightSeriesStore[A]
trait LeftSeriesArray[A] extends SeriesArray[A] with LeftSeriesStore[A]

 /**
  * Abstract Array base class
  *
  * @param capacity buffer capacity
  * @tparam A buffer element type
  */
abstract class BaseCursorArray[A]() 
				extends collection.immutable.IndexedSeq[A] 
				with SeriesArray[A] {

  protected val arrdata = new ArrayBuffer[A]()
  protected def data = arrdata
  protected var idx : Int = 0
  protected def shiftCursor = { idx = cursorToRight(idx); }
  
  def cursor = idx
  def another : BaseCursorArray[A]
  
  def +=(elem: A): this.type = {
    data += elem
    shiftCursor
    this
  }
  def ++=(col: Traversable[A]) : this.type = { col.foreach(e => this += e); this }
}

class RightArray[A]() extends BaseCursorArray[A] with RightSeriesArray[A] {
  
	class RightArrayRef[A](d: IndexedSeq[A], i: Int)	extends BaseSeriesArrayRef[A](d,i) 
														with RightSeriesRef[A] {
	  def apply(i: Int) = d(Math.max(0,i))
	  def view(offset: Int = 0) = new RightArrayRef(d, idx - Math.abs(offset))
	  def next = { idx = Math.min(cursorToRight(idx),data.size); this }
	}
  
  def apply(i: Int) = data(Math.max(0,i))
  def view(offset: Int = 0) = new RightArrayRef(arrdata,idx - Math.abs(offset))
  def another = new RightArray[A]()
}


class LeftArray[A]() extends BaseCursorArray[A] with LeftSeriesArray[A] {
  
	class LeftArrayRef[A](d: IndexedSeq[A], i: Int) extends BaseSeriesArrayRef[A](d,i) with LeftSeriesRef[A] {
	  def apply(i: Int) = data(Math.max(0,size - i - 1 % size))
	  def next = { idx = Math.min(cursorToRight(idx),data.size); this }
	  def view(offset: Int = 0) = new LeftArrayRef(d, idx - Math.abs(offset))
	}
  
  def apply(i: Int) = data(Math.max(0,data.size - i - 1 % data.size))
  def view(offset: Int = 0) = new LeftArrayRef(arrdata,idx - Math.abs(offset))
  def another = new RightArray[A]()
}

