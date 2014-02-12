package bitzguild.scollection.function

import bitzguild.scollection.LeftSeq
import bitzguild.scollection.transform.LeftDoublesFunction
import bitzguild.scollection.transform._

/**
 * Simple Moving Average
 */
class SimpleMovingAverage extends LeftDoublesFunction {
  def init(domain: LeftSeq[Double]) = {}
  def apply(domain: LeftSeq[Double]) = domain.sum / domain.size
}

/**
 * Exponential Moving Average
 */
class ExponentialMovingAverage(val length: Int) extends LeftDoublesFunction {
  val alpha : Double = 2.0 / (1 + length)
  var prior = 0.0
  def init(domain: LeftSeq[Double]) = { prior = domain(0) }
  def apply(domain: LeftSeq[Double]) = (alpha * (domain(0) - prior)) + prior
}

/**
 * Double Smoothed Exponential Moving Average (a.k.a. Dema). Member of
 * the so-called 'zero-lag' averages, which counter lag effect through
 * some mathematical adjustment.
 * <br>
 * <code>(2 * XAverage(Price, Len)) - (XAverage(XAverage(Price, Len), Len))</code>
 */
class DoublesSmoothedMovingAverage(val length: Int) extends LeftDoublesFunction {
  var xma : LeftSeq[Double] = null
  var xma2 : LeftSeq[Double] = null
  def init(domain: LeftSeq[Double]) = {
    xma = DoubleFunctions.xma(domain, length)
    xma2 = DoubleFunctions.xma(xma, length)
  }
  def apply(domain: LeftSeq[Double]) = (2 * xma(0)) - xma2(0)  
}

/**
 * Triple Smoothed Exponential Moving Average (a.k.a. Tema). Member of
 * the so-called 'zero-lag' averages, which counter lag effect through
 * some mathematical adjustment.
 * <br>
 * <code>(3 * XAverage(Price, Len)) - (3 * XAverage(XAverage(Price, Len), Len)) + XAverage(XAverage(XAverage(Price, Len), Len), Len)</code>
 */
class TripleSmoothedMovingAverage(length: Int) extends DoublesSmoothedMovingAverage(length) {
  var xma3 : LeftSeq[Double] = null
  override def init(domain: LeftSeq[Double]) = {
    super.init(domain)
    xma3 = DoubleFunctions.xma(xma2, length)
  }
  override def apply(domain: LeftSeq[Double]) =  (3 * xma(0)) - (3 * xma2(0)) + xma3(0)
}

/**
 * Infinite Impulse Filter with compensated lag. This function
 * uses a fixed-length look back of three events. Reference is
 * July 2002 article in Technical Analysis of Stocks & Commodities,
 * by John F. Ehlers. The aim of so-called 'zero-lag' functions
 * is to counter effect of filtering lag through mathematical
 * adjustment.
 */
class InfiniteImpulseFilter3Pole(val length: Int) extends LeftDoublesFunction {
  var prior = 0.0
  def init(domain: LeftSeq[Double]) = { prior = domain(0) }
  def apply(domain: LeftSeq[Double]) = 0.2*(2.0*domain(0) - domain(3)) + 0.8*prior
}

/**
 * Infinite Impulse Filter with compensated lag. This function
 * uses a calculated N-period look back like the XMA. Reference is
 * July 2002 article in Technical Analysis of Stocks & Commodities,
 * by John F. Ehlers. The aim of so-called 'zero-lag' functions
 * is to counter effect of filtering lag through mathematical
 * adjustment.
 */
class InfiniteImpulseFilterNPole(val length: Int) extends LeftDoublesFunction {
  val alpha : Double = 2.0 / (1 + length)
  val lag : Int = ((1.0/alpha) - 1).toInt
  var prior = 0.0
  def init(domain: LeftSeq[Double]) = { prior = domain(0) }
  def apply(domain: LeftSeq[Double]) =  alpha*(2.0*domain(0) - domain(lag)) + (1.0 - alpha)*prior
}

/**
 * Finite Impulse Filter with compensated lag. This function
 * uses a fixed-length look back of five events. Reference is
 * July 2002 article in Technical Analysis of Stocks & Commodities,
 * by John F. Ehlers. The aim of so-called 'zero-lag' functions
 * is to counter effect of filtering lag through mathematical
 * adjustment.
 */
class FiniteImpulseFilter5Pole(val length: Int) extends LeftDoublesFunction {
  def init(domain: LeftSeq[Double]) = { }
  def apply(domain: LeftSeq[Double]) =  (domain(0) + 2.0*domain(1) + 3.0*domain(2) + 3.0*domain(3) + 2.0*domain(4) + domain(5))/12.0
}

/**
 * Finite Impulse Filter with compensated lag. This function
 * uses a fixed-length look back of six events. Reference is
 * July 2002 article in Technical Analysis of Stocks & Commodities,
 * by John F. Ehlers. The aim of so-called 'zero-lag' functions
 * is to counter effect of filtering lag through mathematical
 * adjustment.
 */
class FiniteImpulseFilter6Pole(val length: Int) extends LeftDoublesFunction {
  def init(domain: LeftSeq[Double]) = { }
  def apply(domain: LeftSeq[Double]) =  (domain(0) + 3.5*domain(1) + 4.5*domain(2) + 3*domain(3) + 0.5*domain(4) - 0.5*domain(5) - 1.5*domain(6))/10.5
}

/**
 * Second Order Moving Average as found in January 2000, TASC and adapted from
 * TradeStation implementation. This function aims to produce a moving average using the 
 * simple moving average (Sma), and an adjustment factor to compensate for lag error. 
 * The adjustment factor is based on applying the 2nd order derivative.
 */
class SecondOrderMovingAverage(val length: Int) extends LeftDoublesFunction {
  var sma : LeftSeq[Double] = null
  def init(domain: LeftSeq[Double]) = { sma = DoubleFunctions.sma(domain, length) }
  def apply(domain: LeftSeq[Double]) =  {
    var slope = 0.0
    var factor = 1.0
    for(i <- 0 until length) {
    	val ii = length - i - 1
    	factor = 1.0 + (2.0*i)
    	slope = slope + domain(ii)*((length - factor)/2.0)
    }
    sma(0) + (6.0 * slope)/((length + 1)*length)
  }
}

