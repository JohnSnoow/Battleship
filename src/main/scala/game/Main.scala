package game

import Prompt._
import scala.annotation.tailrec
import scala.util.Random
import scala.collection.mutable.HashMap

/**
* @param gameMode the game mode : 1 = Player vs Player, 2 = Player vs AI, 3 = AI vs AI
* @param firstPlayer the player that will fire the first shot. 0 = first player to register, 1 = second player to register
* @param currentPlayer the player whose turn it is
* @param playerOne the first player to be created
* @param playerTwo the second player to be created
* @param gameProgress the current phase of the game : 0 = Choosing game mode, 01 = Choosing first AI, 02 = Choosing second AI, 1 = Placing boats, 2 = Playing the game, 3 = game ends
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
                + "1 = Player vs Player, 2 = Player vs AI, 3 = AI vs AI 100 times \n"
                + "you can also (q)uit the game\n")
            val userInput = getUserInput("gameMode").getOrElse("tryAgain")
            // handle the result
            userInput match {
                case "1" => {
                    mainLoop(gameState.copy(gameMode = 1, gameProgress = "00"), random)
                }
                case "2" => {
                    mainLoop(gameState.copy(gameMode = 2, gameProgress = "00"), random)
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
            // user(s) enter their name(s)
            case "00" =>
            if (gameState.gameMode == 1) {
                showPrompt(
                    "Player 1 : enter your username \n"
                    + "you can also (q)uit the game \n")
                val userInputPlayer1 = getUserInput("userName").getOrElse("Q")
                userInputPlayer1 match {
                    case "Q" => 
                        printGameOver()
                    case _ => 
                        showPrompt(
                            "Player 2 : enter your username \n"
                            + "you can also (q)uit the game \n")
                        val userInputPlayer2 = getUserInput("userName").getOrElse("Q")
                        userInputPlayer2 match {
                            case "Q" => 
                                printGameOver()
                            case _ => 
                            mainLoop(gameState.copy(
                                playerOne = Some(Human(userInputPlayer1, 0, Grid(HashMap[String, Cell]()), Grid(HashMap[String, Cell]()), List[Boat]() )), 
                                playerTwo = Some(Human(userInputPlayer2, 1, Grid(HashMap[String, Cell]()), Grid(HashMap[String, Cell]()), List[Boat]() )),
                                gameProgress = "1" )
                            , random)
                        }
                }
            } else if (gameState.gameMode == 2) {
                showPrompt(
                    "Enter your username \n"
                    + "you can also (q)uit the game \n")
                val userInput = getUserInput("userName").getOrElse("Q")
                userInput match {
                    case "Q" => 
                        printGameOver()
                    case _ => 
                        mainLoop(gameState.copy(playerOne = Some(Human(userInput, 0, Grid(HashMap[String, Cell]()), Grid(HashMap[String, Cell]()), List[Boat]() )), 
                        gameProgress = "01" ), random)
                }
            } else printGameOver()
            
            // Choose first AI difficulty 
            case "01" =>
            showPrompt(
                "Choose the difficulty for AI \n"
                + "(e)asy, (m)edium or (h)ard \n"
                + "you can also (q)uit the game \n")
            val userInput = getUserInput("AIDifficulty").getOrElse("tryAgain")
            val listBoats = List(Boat("carrier", 5, List[Cell]()), Boat("battleship", 4, List[Cell]()), Boat("cruiser", 3, List[Cell]()), Boat("submarine", 3, List[Cell]()), Boat("destroyer", 2, List[Cell]()))
            userInput match {
                case "E" => 
                    // create easy AI
                    val newAI = AIEasy("AIEasy", 1, Grid(HashMap[String, Cell]()), Grid(HashMap[String, Cell]()), listBoats)
                    if (gameState.gameMode == 2) mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random) else mainLoop(gameState.copy(gameProgress = "02", playerOne = Some(newAI)), random)
                case "M" =>
                    // create medium AI
                    val newAI = AIMedium("AIMedium", 1, Grid(HashMap[String, Cell]()), Grid(HashMap[String, Cell]()), listBoats)
                    if (gameState.gameMode == 2) mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random) else mainLoop(gameState.copy(gameProgress = "02", playerOne = Some(newAI)), random)
                case "H" =>
                    // create hard AI
                    val newAI = AIHard("AIHard", 1, Grid(HashMap[String, Cell]()), Grid(HashMap[String, Cell]()), listBoats)
                    if (gameState.gameMode == 2) mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random) else mainLoop(gameState.copy(gameProgress = "02", playerOne = Some(newAI)), random)
                case "tryAgain" => 
                    mainLoop(gameState, random)
                case _ => 
                    printGameOver()
                    // return out of the recursion here
            }
            // Choose second AI difficulty 
            case "02" =>
            showPrompt(
                "Choose the difficulty for second AI \n"
                + "(e)asy, (m)edium or (h)ard \n"
                + "you can also (q)uit the game\n")
            val userInput = getUserInput("AIDifficulty").getOrElse("tryAgain")
            val listBoats = List(Boat("carrier", 5, List[Cell]()), Boat("battleship", 4, List[Cell]()), Boat("cruiser", 3, List[Cell]()), Boat("submarine", 3, List[Cell]()), Boat("destroyer", 2, List[Cell]()))
            userInput match {
                case "E" => 
                    // create easy AI
                    val newAI = AIEasy("AIEasy", 2, Grid(HashMap[String, Cell]()), Grid(HashMap[String, Cell]()), listBoats)
                    mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random)
                case "M" => 
                    // create medium AI
                    val newAI = AIMedium("AIMedium", 2, Grid(HashMap[String, Cell]()), Grid(HashMap[String, Cell]()), listBoats)
                    mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random)
                case "H" => 
                    // create hard AI
                    val newAI = AIHard("AIHard", 2, Grid(HashMap[String, Cell]()), Grid(HashMap[String, Cell]()), listBoats)
                    mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random)
                case "tryAgain" => 
                    mainLoop(gameState, random)
                case _ => 
                    printGameOver()
                    // return out of the recursion here
            }
            // Placing boats
            case "1" => 
                val listBoats: List[Boat] = List(Boat("carrier", 5, List[Cell]()), Boat("battleship", 4, List[Cell]()), Boat("cruiser", 3, List[Cell]()), Boat("submarine", 3, List[Cell]()), Boat("destroyer", 2, List[Cell]()))
                if (gameState.gameMode == 1) {
                    val newPlayerOne: Human = Human.placeBoats(Human( gameState.playerOne.get.name, 0, Grid(HashMap[String, Cell]()), Grid(HashMap[String, Cell]()), List[Boat]() ), listBoats, random)
                    val newPlayerTwo: Human = Human.placeBoats(Human( gameState.playerTwo.get.name, 1, Grid(HashMap[String, Cell]()), Grid(HashMap[String, Cell]()), List[Boat]() ), listBoats, random)
                    mainLoop(gameState.copy(gameProgress = "2", playerOne= Some(newPlayerOne), playerTwo = Some(newPlayerTwo)), random)
                } else if (gameState.gameMode == 2) {
                    val newPlayerOne: Human = Human.placeBoats(Human( gameState.playerOne.get.name, 0, Grid(HashMap[String, Cell]()), Grid(HashMap[String, Cell]()), List[Boat]() ), listBoats, random)
                    val newAI: Player = Player.placeBoatsRandom( gameState.playerTwo.get, listBoats, random, 0)
                    newAI.ownedGrid.showGrid()
                    mainLoop(gameState.copy(gameProgress = "2", playerOne= Some(newPlayerOne), playerTwo = Some(newAI)), random)
                } else {
                    val newAIOne: Player = Player.placeBoatsRandom( gameState.playerOne.get, listBoats, random, 0)
                    val newAITwo: Player = Player.placeBoatsRandom( gameState.playerTwo.get, listBoats, random, 0)
                    newAIOne.ownedGrid.showGrid()
                    newAITwo.ownedGrid.showGrid()
                    mainLoop(gameState.copy(gameProgress = "2", playerOne= Some(newAIOne), playerTwo = Some(newAITwo)), random)
                }
            // Playing game
            case "2" => 
            // current player takes a shot. If AI no prompt
            // if (gameState.currentPlayer == 0) mainLoop(gameState.copy(currentPlayer = 1), random) else mainLoop(gameState.copy(currentPlayer = 0), random)
            // Game Ends with a winner
            // check if boat is hit
            // if ( opponent.ownedGrid.isHit(Cell(rowR, colR, false, false)) ) showPrompt("Hit !") else showPrompt("Miss !")
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
