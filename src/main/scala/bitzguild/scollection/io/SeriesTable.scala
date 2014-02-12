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
  def renderName : String
  def renderValue(index: Int) : String 
}

class Row(index: Int, columns: Array[Column], val columnSeparator: String = " ") {
  def renderHeader = columns.map(c => c.renderName).reduce(_ + columnSeparator + _) 
  def renderRow = columns.map(c => c.renderValue(index)).reduce(_ + columnSeparator + _)
  def next = new Row(index+1, columns, columnSeparator)
  override def toString = renderRow
}

object Row {
  def apply(index: Int, columns: Array[Column], columnSeparator: String = " ") = new Row(index, columns, columnSeparator)
}


/**
 * Type-erasure does not allow for generic add() signature because LeftSeq all appear the same to the JVM after type-erasure
 * Must implement 
 */
class Table(var columns: Option[Array[Column]]) {
  import java.io.PrintStream
  
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
  
  def top(n: Int, ps: PrintStream = System.out) : Unit = {
    ps.println(Row(0,columns.get," ").renderHeader)
    for(i <- 0 until n) ps.println((Row(i,columns.get," ")).toString)
  }
    
  def page(page: Int, chunk: Int, p: Int, ps: PrintStream = System.out) = 
    for(i <- (page*chunk) until (page*(chunk + 1))) ps.println((Row(i,columns.get," ")).toString)
  
}

abstract class BaseColumn[A](val name: String, val columnWidth: Int, val align: CellAlignment) extends Column {
  def renderName = renderNameInto(new StringBuffer).toString
  def renderValue(index: Int) : String = renderValueInto(index, new StringBuffer).toString
  def renderNameInto(sb: StringBuffer) : StringBuffer = {
	val iStart = sb.length(); sb.append(name);
	val iEnd = sb.length()
	val iwidth = iEnd - iStart
	val ipad = columnWidth - (iEnd - iStart)
	if (iwidth <= columnWidth) pad(ipad,sb,iStart,iEnd)
	else { sb.setLength(iStart + columnWidth - 3); sb.append("...") }
    sb
  }
  def renderValueInto(index: Int, sb: StringBuffer) : StringBuffer = {
	val iStart = sb.length()
	renderImpl(index, sb)
	val iEnd = sb.length();
	val iwidth = iEnd - iStart;
	val ipad = columnWidth - (iEnd - iStart);
	if (iwidth <= columnWidth) pad(ipad,sb,iStart,iEnd)
	else { sb.setLength(iStart + columnWidth - 3); sb.append("...") }
    sb
  }
  def pad(ipad: Int, sb: StringBuffer, iStart: Int, iEnd: Int) = align match {
    case AlignLeft()	=> for(i <- 1 to ipad) sb.insert(iEnd," ")
    case AlignRight() 	=> for(i <- 1 to ipad) sb.insert(iStart," ")
    case AlignCenter() 	=> {
		val ipadR = ipad / 2;
		val ipadL = ipad - ipadR;
		for(i <- 1 to ipadL) sb.insert(iEnd," ")
		for(i <- 1 to ipadR) sb.insert(iStart," ")
    }
  }
  protected def renderImpl(index: Int, sb: StringBuffer)
}

class NumericColumn[N : Numeric](val series: LeftSeq[N], name: String, columnWidth: Int, align: CellAlignment) 
  extends BaseColumn[N](name,columnWidth,align) {
  protected def renderImpl(index: Int, sb: StringBuffer) = sb.append(series(index))
}

class DerivedNumericColumn[N : Numeric](val series: LeftSeq[N], val fn: N => N, name: String, columnWidth: Int, align: CellAlignment) 
  extends BaseColumn[N](name,columnWidth,align) {
  protected def renderImpl(index: Int, sb: StringBuffer) = sb.append(fn(series(index)))
}

class GenericColumn[A](val series: LeftSeq[A], name: String, columnWidth: Int, align: CellAlignment) 
  extends BaseColumn[A](name,columnWidth,align) {
  protected def renderImpl(index: Int, sb: StringBuffer) = sb.append(series(index))
}

class BooleanColumn(val series: LeftSeq[Boolean], name: String, columnWidth: Int, align: CellAlignment) 
  extends BaseColumn[Boolean](name,columnWidth,align) {
  protected def renderImpl(index: Int, sb: StringBuffer) = sb.append(series(index))
}

class FormattedDoubleColumn(val series: LeftSeq[Double], name: String, formatStr: String = "#.000", columnWidth: Int, align: CellAlignment) 
  extends BaseColumn[Double](name,columnWidth,align) {
  import java.text.{DecimalFormat, FieldPosition,NumberFormat}
  private val decimalFormat = new DecimalFormat(formatStr) 
  private val fieldPosition = new FieldPosition(NumberFormat.FRACTION_FIELD)
  protected def renderImpl(index: Int, sb: StringBuffer) = decimalFormat.format(series(index), sb, fieldPosition)
}







