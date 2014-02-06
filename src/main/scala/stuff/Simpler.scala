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

package stuff

import scala.collection.mutable.ArrayBuffer

trait OldCursorSeq[A] extends IndexedSeq[A] {
//  def cursor : Long
  def next : OldCursorSeq[A]
  def data : collection.IndexedSeq[A]
  def length = data.length
  def view(offset: Int) : OldCursorSeq[A]
  def flip : OldCursorSeq[A]
}

abstract class BaseCursorWrap[A](val indata: IndexedSeq[A], offset: Long = 0L) extends OldCursorSeq[A] {
  protected var idx = offset
  def next = { shiftRight; this }
  def data = indata
  protected def shiftRight = { idx = (idx + 1) % data.size } 
}





class Wrap[A](indata: IndexedSeq[A], offset: Long = 0L) extends BaseCursorWrap[A](indata,offset) {
  def apply(index: Int) = data(index)
  def view(offset: Int) = new Wrap(indata,idx+offset)
  def flip = 
    if (data.isInstanceOf[Flip[A]]) data.asInstanceOf[OldCursorSeq[A]]
    else new Flip(indata,idx)
//  def +=(elem: A): this.type = {
//    indata += elem
//    this
//  }
}

class Flip[A](in: IndexedSeq[A], offset: Long = 0L) extends BaseCursorWrap[A](in,offset) {
  def apply(i: Int) = data(data.size - i - 1 % data.size)
  def view(offset: Int) = new Flip(indata,idx+offset)
  def flip = 
    if (data.isInstanceOf[Wrap[A]]) data.asInstanceOf[OldCursorSeq[A]]
    else new Wrap(indata,idx)
}

class RView[A](val data: IndexedSeq[A], val offset: Int, len: Int) extends IndexedSeq[A] {
  def length = len
  def apply(index: Int) = data(offset + index)
}






