package game

import Prompt._
// import Player._
import scala.annotation.tailrec
import scala.util.Random

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
* @gameState
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
                + "you can also (q)uit the game\n")
            val userInput = getUserInput("IADifficulty").getOrElse("tryAgain")
            userInput match {
                case "E" => 
                    // create easy IA
                    val newAI = AI("AI", 1)
                    mainLoop(gameState.copy(gameProgress = "1", playerOne = Some(newAI)), random)
                case "M" =>
                    // create medium IA
                    val newAI = AI("AI", 1)
                    mainLoop(gameState.copy(gameProgress = "1", playerOne = Some(newAI)), random)
                case "H" =>
                    // create hard IA
                    val newAI = AI("AI", 1)
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
            userInput match {
                case "E" => 
                    // create easy IA
                    val newAI = AI("AI", 2)
                    mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random)
                case "M" => 
                    // create medium IA
                    val newAI = AI("AI", 2)
                    mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random)
                case "H" => 
                    // create hard IA
                    val newAI = AI("AI", 2)
                    mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random)
                case "tryAgain" => 
                    mainLoop(gameState, random)
                case _ => 
                    printGameOver()
                    // return out of the recursion here
            }
            // Placing boats
            case "1" => 
            // Playing game
            case "2" =>
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
