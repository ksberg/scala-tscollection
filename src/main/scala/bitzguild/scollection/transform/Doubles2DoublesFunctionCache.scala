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

package bitzguild.scollection.transform

//import bitzguild.scollection.{CursorSeq,LeftBuffer}
//import bitzguild.collection.{Wrap,Flip}
//import bitzguild.collection.transform.Doubles2DoublesFunction

//type Doubles2 = CursorSeq[Double]

class Doubles2DoublesFunctionCache

//class Doubles2DoublesFunctionCache(val function: Doubles2DoublesFunction, val domain: List, len: Int) extends Doubles {
//  val lookback =  Math.abs(len)
//  val domainview = domain.view(0)
//  val range = new LeftBuffer[Double]()
//
//  def cursor = 0
//  def next = { this }
//  def data = domain
//  def view(offset: Int) = new Wrap(this)
//  def flip = new Flip(this)
//  def apply(index: Int) = {
//    // if range.size == 0 ... function.calculateInitial() and loop lookback times (???)
//    while(domainview.cursor != domain.cursor) {
////      range += function.calculateEntry(domainview, range, lookback)
//      domainview.next
//    }
//    domain(index)
//  }
//}
