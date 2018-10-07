package game

import Prompt._
// import Player._
import scala.annotation.tailrec
import scala.util.Random
import scala.collection.mutable.HashMap

/**
* @param gameMode the game mode : 1 = Player vs Player, 2 = Player vs IA, 3 = IA vs IA
* @param firstPlayer the player that will fire the first shot. 0 = first player to register, 1 = second player to register
* @param currentPlayer the player whose turn it is
* @param playerOne the first player to be created
* @param playerTwo the second player to be created
* @param gameProgress the current phase of the game : 0 = Choosing game mode, 01 = Choosing first IA, 02 = Choosing second IA, 1 = Placing boats, 2 = Playing the game, 3 = game ends
*/
case class GameState(gameMode: Int, firstPlayer: Int, currentPlayer: Int, playerOne: Option[Player], playerTwo: Option[Player], gameProgress: String)

/**
* @param gameState
* @param random 
*/
object Main extends App {

    val r = Random
    val s = GameState(0, 0, 0, None, None, "0")
    mainLoop(s, r)

    @tailrec
    def mainLoop(gameState: GameState, random: Random) {
        gameState.gameProgress match {
            // Choosing game Mode
            case "0" => 
            showPrompt(
                "Welcome to Battleships. \n"
                + "Please choose the game mode \n"
                + "1 = Player vs Player, 2 = Player vs IA, 3 = IA vs IA \n"
                + "you can also (q)uit the game\n")
            val userInput = getUserInput("gameMode").getOrElse("tryAgain")
            // handle the result
            userInput match {
                case "1" => {
                    mainLoop(gameState.copy(gameMode = 1, gameProgress = "1"), random)
                }
                case "2" => {
                    mainLoop(gameState.copy(gameMode = 2, gameProgress = "01"), random)
                }
                case "3" => {
                    mainLoop(gameState.copy(gameMode = 3, gameProgress = "01"), random)
                }
                case "tryAgain" => {
                    mainLoop(gameState, random)
                }
                case _ => {
                    printGameOver()
                    // return out of the recursion here
                }
            }
            // Choose first IA difficulty 
            case "01" =>
            showPrompt(
                "Choose the difficulty for IA \n"
                + "(e)asy, (m)edium or (h)ard \n"
                + "you can also (q)uit the game \n")
            val userInput = getUserInput("IADifficulty").getOrElse("tryAgain")
            val listBoats = List(Boat("carrier", 5, List[Cell]()), Boat("battleship", 4, List[Cell]()), Boat("cruiser", 3, List[Cell]()), Boat("submarine", 3, List[Cell]()), Boat("destroyer", 2, List[Cell]()))
            userInput match {
                case "E" => 
                    // create easy IA
                    val newAI = AIEasy("AIEasy", 1, Grid(new HashMap[String, Cell]), Grid(new HashMap[String, Cell]), listBoats)
                    mainLoop(gameState.copy(gameProgress = "1", playerOne = Some(newAI)), random)
                case "M" =>
                    // create medium IA
                    val newAI = AIMedium("AIMedium", 1, Grid(new HashMap[String, Cell]), Grid(new HashMap[String, Cell]), listBoats)
                    mainLoop(gameState.copy(gameProgress = "1", playerOne = Some(newAI)), random)
                case "H" =>
                    // create hard IA
                    val newAI = AIHard("AIHard", 1, Grid(new HashMap[String, Cell]), Grid(new HashMap[String, Cell]), listBoats)
                    mainLoop(gameState.copy(gameProgress = "1", playerOne = Some(newAI)), random)
                case "tryAgain" => 
                    mainLoop(gameState, random)
                case _ => 
                    printGameOver()
                    // return out of the recursion here
            }
            // Choose second IA difficulty 
            case "02" =>
            showPrompt(
                "Choose the difficulty for second IA \n"
                + "(e)asy, (m)edium or (h)ard \n"
                + "you can also (q)uit the game\n")
            val userInput = getUserInput("IADifficulty").getOrElse("tryAgain")
            val listBoats = List(Boat("carrier", 5, List[Cell]()), Boat("battleship", 4, List[Cell]()), Boat("cruiser", 3, List[Cell]()), Boat("submarine", 3, List[Cell]()), Boat("destroyer", 2, List[Cell]()))
            userInput match {
                case "E" => 
                    // create easy IA
                    val newAI = AIEasy("AIEasy", 2, Grid(new HashMap[String, Cell]), Grid(new HashMap[String, Cell]), listBoats)
                    mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random)
                case "M" => 
                    // create medium IA
                    val newAI = AIMedium("AIMedium", 2, Grid(new HashMap[String, Cell]), Grid(new HashMap[String, Cell]), listBoats)
                    mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random)
                case "H" => 
                    // create hard IA
                    val newAI = AIHard("AIHard", 2, Grid(new HashMap[String, Cell]), Grid(new HashMap[String, Cell]), listBoats)
                    mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random)
                case "tryAgain" => 
                    mainLoop(gameState, random)
                case _ => 
                    printGameOver()
                    // return out of the recursion here
            }
            // Placing boats
            case "1" => 
            // placeBoatLoop(player: Player, boats: List[Boat]) 
            // listBoats = [Boat("carrier", 5, None), Boat("battleship", 4, None), Boat("cruiser", 3, None), Boat("submarine", 3, None), Boat("destroyer", 2, None)]
            // placeBoatsLoop(gameState.playerOne, listBoats, 0) 
            // If and only if playerOne is human player match { case Human(name, number) => prompt case AI(name, number) => pas de prompt}
            // placeBoatsLoop(gameState.playerTwo, listBoats, 0) If and only if playerTwo is human
            // mainloop(gameState.copy(gameProgress = "2", playerTwo = newPlayerTwo, playerOne = newPlayerOne), random)
                @tailrec
                def placeBoatsLoop(player: Player, boats: List[Boat], progress: Int, random: Random) {
                    if (progress < 5)
                        showPrompt(
                            "Select a collumn to place your boat : \n"
                            + "A-J \n")
                        val userInputCol = getUserInput("placeCol").getOrElse("tryAgain")
                        showPrompt(
                            "Select a row to place your boat : \n"
                            + "1-10 \n")
                        val userInputRow = getUserInput("placeRow").getOrElse("tryAgain")
                        showPrompt(
                            "Place your boat (h)orizontally or (v)ertically \n")
                        val userInputDir = getUserInput("h or v").getOrElse("tryAgain")
                        if (userInputCol == "tryAgain" || userInputRow == "tryAgain" || userInputDir == "tryAgain") placeBoatsLoop(player, boats, progress, random)
                        else placeBoatsLoop(player, boats, progress+1, random) // + add place boat on Grid and save in player
                }
            // Playing game
            case "2" => 
            // current player takes a shot. If AI no prompt
            // if (gameState.currentPlayer == 0) mainLoop(gameState.copy(currentPlayer = 1), random) else mainLoop(gameState.copy(currentPlayer = 0), random)
            // Game Ends with a winner
            case "3" => gameState.currentPlayer match {
                case 0 => printGameOver(gameState.playerOne.get)
                case 1 => printGameOver(gameState.playerTwo.get)
                }  
                showPrompt(
                    "Do you wish to play again ? \n"
                    + "(y)es or (n)o")
                val userInput = getUserInput("remakeGame").getOrElse("wrongInput")
                userInput match {
                case "y" => 
                    var newFirst = if (gameState.firstPlayer == 0 ) 1 else 0
                    // if (gameState.firstPlayer == 0 ) { val newFirst = 1 } else { val newFirst = 0 }
                    mainLoop(s.copy(firstPlayer = newFirst), random)
                case _ =>
                    showPrompt("Exiting Battleship.\n")
                }
            // default behavior
            case _ => showPrompt(
                "There was an error. \n" 
                + "relaunching the game \n")
                mainLoop(s, random)

        }
        
    }

}
