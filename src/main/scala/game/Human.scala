package game

case class Human(val name: String, val number: Int, val ownedGrid: Grid, val opponentGrid: Grid, val boats: List[Boat]) extends Player() {
    def shoot(opponent: Player, random: Random): (Player, Player) = {
      opponentGrid.showGrid()
      showPrompt( "Select a collumn to shoot : \n" + "A-J \n" )
      val userInputCol = getUserInput("askCol").getOrElse("tryAgain")
      showPrompt( "Select a row to shoot : \n" + "1-10 \n" )
      val userInputRow = getUserInput("askRow").getOrElse("tryAgain")
      if (userInputCol == "tryAgain" || userInputRow == "tryAgain") shoot(opponent, random)
      else {
        val colR: String = Convert_Util.gridCollumns(random.nextInt(10)).toString
        val rowR: Int = random.nextInt(10)
      }
      // to finish
      (Human(name, number, ownedGrid, opponentGrid, boats), opponent)
    }


}

object Human {
    def placeBoats(human: Human, listBoats: List[Boat], random: Random): (Human) = {
        @tailrec
        def placeBoatsLoop(player: Human, boats: List[Boat], progress: Int, random: Random) : Human = {
            if (progress < 5) {
                player.ownedGrid.showGrid()
                showPrompt(
                    "Player " + player.name + "\n"
                    + "Boat to place :" + boats(progress).name + " (length : " + boats(progress).length + " )" + "\n")
                showPrompt(
                    "Select a collumn to place your boat : \n"
                    + "A-J \n")
                val userInputCol = getUserInput("askCol").getOrElse("tryAgain")
                showPrompt(
                    "Select a row to place your boat : \n"
                    + "1-10 \n")
                val userInputRow = getUserInput("askRow").getOrElse("tryAgain")
                showPrompt(
                    "Place your boat (h)orizontally or (v)ertically \n")
                val userInputDir = getUserInput("h or v").getOrElse("tryAgain")
                if (userInputCol == "tryAgain" || userInputRow == "tryAgain" || userInputDir == "tryAgain") placeBoatsLoop(player, boats, progress, random)
                else {
                    val (newBoat, success): (Boat, Boolean) = player.tryPlaceBoat(boats(progress),  userInputCol, userInputRow.toInt, userInputDir, 0, List[Cell]())
                    if (success) {
                        val newGrid: Grid = Player.addboatTogrid(player.ownedGrid, newBoat, 0)
                        placeBoatsLoop(player.copy(boats = newBoat :: player.boats, ownedGrid = newGrid), boats, progress+1, random)
                    } else {
                        placeBoatsLoop(player, boats, progress, random)
                    }
                }
            } else player
        }

        placeBoatsLoop(human, listBoats, 0, random)
    }
}
