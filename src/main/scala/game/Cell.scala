package game


case class Cell(row: Int, col: String, hit: Boolean, occupied: Boolean)

object Cell {
    def apply(row: Int, col: String): Cell = new Cell(row, col, false, false)
    def apply(row: Int, col: Int): Cell = new Cell(row, Convert_Util.gridCollumns(col).toString, false, false)
}