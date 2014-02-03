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

import scala.collection.IndexedSeq

object SeriesDefaults {
   var capacity = 3
}

trait BaseSeries[A] extends IndexedSeq[A] {
  def cursor : Int
  protected def cursorMax : Int
  protected def cursorToLeft(i: Int)	= if (i == 0) cursorMax else i - 1  
  protected def cursorToRight(i: Int) = if (i == cursorMax) 0 else i + 1
  protected def data : collection.IndexedSeq[A]	
}
 
trait MutableSeries[A] {
  def +=(elem: A): MutableSeries[A]
  def ++=(col: Traversable[A]) : MutableSeries[A]
}

trait SeriesRef[A] extends BaseSeries[A]{
  def next : SeriesRef[A]
}

trait LeftRightSeries[A] {
  def zeroIsLast : Boolean
}

// ------------------------------------------------------------------

trait RightSeries[A] extends BaseSeries[A] {
  def zeroIsLast = false
}

trait LeftSeries[A]	extends BaseSeries[A]{
  def zeroIsLast = true
}

trait RightSeriesStore[A] extends RightSeries[A] with MutableSeries[A] {
  def length = data.length
  def view(offset: Int) : SeriesRef[A]
}

trait LeftSeriesStore[A] extends LeftSeries[A] with MutableSeries[A] {
  def length = data.length
  def view(offset: Int) : SeriesRef[A]
}

// ------------------------------------------------------------------

trait RightSeriesRef[A] extends RightSeries[A] with SeriesRef[A]
trait LeftSeriesRef[A] extends LeftSeries[A] with SeriesRef[A]  




