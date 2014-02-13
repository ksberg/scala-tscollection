package bitzguild.scollection.io

import bitzguild.scollection._

trait CellAlignment
case class AlignLeft()		extends CellAlignment
case class AlignRight() 	extends CellAlignment
case class AlignCenter() 	extends CellAlignment

object CellDefaults {
  var width = 10
  var decimalFormat = "#.00"
}

trait Column {
  def size : Int
  def renderName(padded: Boolean) : String
  def renderValue(index: Int, padded: Boolean) : String 
}

case class Row(index: Int, columns: Array[Column], val padded : Boolean = true, val columnSeparator: String = " ") {
  def renderHeader = columns.map(c => c.renderName(padded)).reduce(_ + columnSeparator + _) 
  def renderRow = columns.map(c => c.renderValue(index,padded)).reduce(_ + columnSeparator + _)
  def next = new Row(index+1, columns, padded, columnSeparator)
  override def toString = renderRow
}


/**
 * Type-erasure does not allow for generic add() signature because LeftSeq all appear the same to the JVM after type-erasure
 * Must implement 
 */
class Table(var columns: Option[Array[Column]]) {
  import java.io.PrintStream
  
  def size : Int = columns.map(a => a.foldLeft(Int.MaxValue)((s,c) => Math.min(s,c.size))) getOrElse 0
  
  def addColumn(c: Column) = columns match {
    case Some(cs) => columns = Some(cs :+ c)
    case None => columns = Some(Array(c))
  }
  def addDoubles(a: LeftSeq[Double], name: String, format: String = CellDefaults.decimalFormat, width: Int = CellDefaults.width, align: CellAlignment = new AlignRight) = 
		  		addColumn(new FormattedDoubleColumn(a,name,format,width,align))
		  		
  def addLongs(seq: LeftSeq[Long],name: String, width: Int = CellDefaults.width, align: CellAlignment = new AlignRight) = 
    			addColumn(new NumericColumn[Long](seq,name,width, align))
    			
  def addInts(seq: LeftSeq[Int], name: String, width: Int = CellDefaults.width, align: CellAlignment = new AlignRight) = 
    			addColumn(new NumericColumn[Int](seq,name,width, align))
    			
  def addBooleans(seq: LeftSeq[Boolean], name: String, width: Int = CellDefaults.width, align: CellAlignment = new AlignRight) =
    			addColumn(new BooleanColumn(seq,name,width, align))
  

  def top(n: Int, ps: PrintStream = System.out) = render(n,ps," ",true)
  def csv(ps: PrintStream) = render(size,ps,",",false) 
  def tsv(ps: PrintStream) = render(size,ps,"\t",false)
  

  def render(n: Int, ps: PrintStream, separator: String, padded: Boolean) = page(0,n,ps,separator,padded)
  def page(page: Int, chunk: Int, ps: PrintStream, separator: String, padded: Boolean) = {
    val last = size
    val start = page*chunk
    val end = Math.min(start + chunk,last)
    ps.println(Row(0,columns.get,padded,separator).renderHeader)
    if (start < last) for(i <- start until end) ps.println((Row(i,columns.get,padded,separator)).toString)
  }

  def json(ps: PrintStream) = {
    "{}"
  }
  
}

abstract class BaseColumn[A](val name: String, val columnWidth: Int, val align: CellAlignment) extends Column {
  def renderName(padded: Boolean) = renderNameInto(new StringBuffer,padded).toString
  def renderValue(index: Int, padded: Boolean) : String = renderValueInto(index, new StringBuffer,padded).toString
  def renderNameInto(sb: StringBuffer, padded: Boolean) : StringBuffer = {
	val iStart = sb.length(); sb.append(name)
	if (padded) {
		val iEnd = sb.length()
		val iwidth = iEnd - iStart
		val ipad = columnWidth - (iEnd - iStart)
		if (iwidth <= columnWidth) pad(ipad,sb,iStart,iEnd)
		else { sb.setLength(iStart + columnWidth - 3); sb.append("...") }
	}
    sb
  }
  def renderValueInto(index: Int, sb: StringBuffer, padded: Boolean) : StringBuffer = {
	val iStart = sb.length()
	renderImpl(index, sb)
	if (padded) {
		val iEnd = sb.length();
		val iwidth = iEnd - iStart;
		val ipad = columnWidth - (iEnd - iStart);
		if (iwidth <= columnWidth) pad(ipad,sb,iStart,iEnd)
		else { sb.setLength(iStart + columnWidth - 3); sb.append("...") }
	}
    sb
  }
  def pad(pad: Int, sb: StringBuffer, start: Int, end: Int) = align match {
    case AlignLeft()	=> for(i <- 1 to pad) sb.insert(end," ")
    case AlignRight() 	=> for(i <- 1 to pad) sb.insert(start," ")
    case AlignCenter() 	=> {
		val ipadR = pad / 2;
		val ipadL = pad - ipadR;
		for(i <- 1 to ipadL) sb.insert(end," ")
		for(i <- 1 to ipadR) sb.insert(start," ")
    }
  }
  protected def renderImpl(index: Int, sb: StringBuffer)
}

class NumericColumn[N : Numeric](val series: LeftSeq[N], name: String, columnWidth: Int, align: CellAlignment)
  extends BaseColumn[N](name,columnWidth,align) {
  def size = series.size
  protected def renderImpl(index: Int, sb: StringBuffer) = sb.append(series(index))
}

class DerivedNumericColumn[N : Numeric](val series: LeftSeq[N], val fn: N => N, name: String, columnWidth: Int, align: CellAlignment) 
  extends BaseColumn[N](name,columnWidth,align) {
  def size = series.size
  protected def renderImpl(index: Int, sb: StringBuffer) = sb.append(fn(series(index)))
}

class GenericColumn[A](val series: LeftSeq[A], name: String, columnWidth: Int, align: CellAlignment) 
  extends BaseColumn[A](name,columnWidth,align) {
  def size = series.size
  protected def renderImpl(index: Int, sb: StringBuffer) = sb.append(series(index))
}

class BooleanColumn(val series: LeftSeq[Boolean], name: String, columnWidth: Int, align: CellAlignment) 
  extends BaseColumn[Boolean](name,columnWidth,align) {
  def size = series.size
  protected def renderImpl(index: Int, sb: StringBuffer) = sb.append(series(index))
}

class FormattedDoubleColumn(val series: LeftSeq[Double], name: String, formatStr: String = "#.000", columnWidth: Int, align: CellAlignment) 
  extends BaseColumn[Double](name,columnWidth,align) {
  import java.text.{DecimalFormat, FieldPosition,NumberFormat}
  def size = series.size
  private val decimalFormat = new DecimalFormat(formatStr) 
  private val fieldPosition = new FieldPosition(NumberFormat.FRACTION_FIELD)
  protected def renderImpl(index: Int, sb: StringBuffer) = decimalFormat.format(series(index), sb, fieldPosition)
}







