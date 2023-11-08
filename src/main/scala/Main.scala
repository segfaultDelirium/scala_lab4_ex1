import scala.compiletime.ops.boolean

case class MyDate(year: Int, month: Int, day: Int) {

  def nextDay(): MyDate = {
    MyDate(year, month, day + 1)
  }

  def +(s: String): String = {
    // s"$year-$month-$day" + s
    toString() + s
  }

  override def toString(): String = {
    s"$year-$month-$day"
  }

  def <(otherDate: MyDate): String = {

    def inner(date: MyDate, otherDate: MyDate): Boolean = {
      if date.year < otherDate.year then {
        return true
      }
      if date.year > otherDate.year then {
        return false
      }
      if date.month < otherDate.month then {
        return true
      }
      if date.month > otherDate.month then {
        return false
      }
      if date.day < otherDate.day then {
        return true
      }
      if date.day > otherDate.day then {
        return false
      }
      return false
    }

    inner(this, otherDate).toString()

  }

  def -(otherDate: MyDate): Int = {
    val monthsDifference = (month - otherDate.month).abs
    val daysDifference = (day - otherDate.day).abs
    monthsDifference * 31 + daysDifference + 1
  }

}

case class MyPeriod(begin: MyDate, end: MyDate) {
  def days: Int = {
    end - begin
  }

  override def toString(): String = {
    s"< $begin : $end >"
  }
}

object test extends App {
  val d1 = MyDate(2015, 10, 10)
  val d2 = MyDate(2015, 11, 10)
  val d3 = d2.nextDay()
  println(d1 + " " + d2 + " " + d3)
  println((d1 < d2) + " " + (d2 < d1))
  val days: Int = d1 - d2
  println("d " + days + " " + (d2 - d1))
  val period = MyPeriod(begin = d1, end = d2)
  println(" " + d1 + " " + d2 + " " + period + " " + period.days)
}
