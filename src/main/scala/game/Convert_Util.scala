package game

import scala.collection.immutable.NumericRange


object Convert_Util {

    // the numeric range used to determine the column from an int
    val gridcolumns: NumericRange[Char] = ('A' to 'J')

    /** strToInt
    * @param str the String to attempt to convert to Int
    * @return an Option[Int], Int if it has been converted, or else None
    */
    def strToInt(str: String): Option[Int] = {
        try {
            Some(str.toInt)
        } catch {
            case e: NumberFormatException => None
        }
    }

    /** indexOfCol
    * @param element a String that is in the numeric range
    * @return an Int, the index of given string in the numeric range
    */
    def indexOfCol (element: String): Int = gridcolumns.indexOf(element.head)
}
