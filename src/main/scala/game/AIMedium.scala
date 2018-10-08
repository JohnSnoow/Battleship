package game

case class AIMedium(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {
    @tailrec
    final def shoot(opponent: Player, random: Random): (Player, Player) = {
        val colR: String = Convert_Util.gridCollumns(random.nextInt(10)).toString
        val rowR: Int = random.nextInt(10)
        if ( opponent.ownedGrid.isHit(Cell(rowR, colR, false, false)) ) shoot(opponent, random)
        else {
            // add shot in grid
            val (newGridOpponentGrid, newCellOpponentGrid): (Grid, Cell) = opponentGrid.cellHit(Cell(rowR, colR, false, false))
            val (newGridOpp, newCellOpp): (Grid, Cell) = opponent.ownedGrid.cellHit(Cell(rowR, colR, false, false))
            if (opponent.number == 1 || opponent.number == 2) {
                opponent.name match {
                    case "AIEasy" =>
                        (AIEasy(name, number, ownedGrid, Grid ( opponentGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpponentGrid ) ), boats),
                        AIEasy(opponent.name, opponent.number, Grid ( opponent.ownedGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpp ) ), opponent.opponentGrid, opponent.boats) )
                    case "AIMedium" =>
                        (AIEasy(name, number, ownedGrid, Grid ( opponentGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpponentGrid ) ), boats),
                        AIMedium(opponent.name, opponent.number, Grid ( opponent.ownedGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpp ) ), opponent.opponentGrid, opponent.boats) )
                    case "AIHard" =>
                        (AIEasy(name, number, ownedGrid, Grid ( opponentGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpponentGrid ) ), boats),
                        AIHard(opponent.name, opponent.number, Grid ( opponent.ownedGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpp ) ), opponent.opponentGrid, opponent.boats) )
                }
            }
            else (AIEasy(name, number, ownedGrid, Grid ( opponentGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpponentGrid ) ), boats),
            Human(opponent.name, opponent.number, Grid ( opponent.ownedGrid.cellGrid.clone() += ( (colR + rowR.toString) -> newCellOpp ) ), opponent.opponentGrid, opponent.boats) )
        }
    }
}
