package game

import scala.collection.immutable.NumericRange


object Convert_Util {
    
    val gridCollumns: NumericRange[Char] = ('A' to 'J')

    def strToInt(str: String): Option[Int] = {
        try {
            Some(str.toInt)
        } catch {
            case e: NumberFormatException => None
        }
    }

    def indexOfCol (element: String): Int = gridCollumns.indexOf(element.head)
}