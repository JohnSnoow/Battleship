package game

/** Class Cell
* @param row the row, an Int that indicate the line the Cell is in (1-10)
* @param col the column, a String that indicate which column the cell is in (A-J)
* @param hit a Boolean that indicates if the cell has been hit
* @param occupied a Boolean that indicates if the cell has a boat on it
*/
case class Cell(row: Int, col: String, hit: Boolean, occupied: Boolean)

object Cell {
    def apply(row: Int, col: String): Cell = new Cell(row, col, false, false)
    def apply(row: Int, col: Int): Cell = new Cell(row, Convert_Util.gridcolumns(col).toString, false, false)
}
