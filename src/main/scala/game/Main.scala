package game

import Prompt._
import scala.annotation.tailrec
import scala.util.Random
import scala.collection.immutable.HashMap

/**
* @param gameMode the game mode : 1 = Player vs Player, 2 = Player vs AI, 3 = AI vs AI 100 times (this game mode diverts from the standart behavior of mainLoop)
* @param firstPlayer the player that will fire the first shot. 0 = first player to register, 1 = second player to register
* @param currentPlayer the player whose turn it is
* @param playerOne the first player to be created
* @param playerTwo the second player to be created
* @param gameProgress the current phase of the game : 0 = Choosing game mode, 00 = player(s) set their name, 01 = Choosing first AI, 02 = Choosing second AI, 1 = Placing boats, 2 = Playing the game, 3 = game ends
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
                                playerOne = Some(Human(
                                  userInputPlayer1,
                                  0,
                                  Grid(HashMap[String, Cell]()),
                                  Grid(HashMap[String, Cell]()),
                                  List[Boat]()
                                )),
                                playerTwo = Some(Human(
                                  userInputPlayer2,
                                  1,
                                  Grid(HashMap[String, Cell]()),
                                  Grid(HashMap[String, Cell]()),
                                  List[Boat]()
                                )),
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
                        mainLoop(
                          gameState.copy(
                            playerOne = Some( Human(
                                userInput,
                                0,
                                Grid(HashMap[String, Cell]()),
                                Grid(HashMap[String, Cell]()),
                                List[Boat]()
                            )),
                            gameProgress = "01" ),
                          random)
                }
            } else printGameOver()

            // Choose first AI difficulty
            case "01" =>
            showPrompt(
                "Choose the difficulty for AI \n"
                + "(e)asy, (m)edium or (h)ard \n"
                + "you can also (q)uit the game \n")
            val userInput = getUserInput("AIDifficulty").getOrElse("tryAgain")
            userInput match {
                case "E" =>
                    // create easy AI
                    val newAI = AIEasy(
                      "AIEasy",
                      1,
                      Grid(HashMap[String, Cell]()),
                      Grid(HashMap[String, Cell]()),
                      List[Boat]())
                    if (gameState.gameMode == 2) mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random) else mainLoop(gameState.copy(gameProgress = "02", playerOne = Some(newAI)), random)
                case "M" =>
                    // create medium AI
                    val newAI = AIMedium(
                      "AIMedium",
                      1,
                      Grid(HashMap[String, Cell]()),
                      Grid(HashMap[String, Cell]()),
                      List[Boat]())
                    if (gameState.gameMode == 2) mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random) else mainLoop(gameState.copy(gameProgress = "02", playerOne = Some(newAI)), random)
                case "H" =>
                    // create hard AI
                    val newAI = AIHard(
                      "AIHard",
                      1,
                      Grid(HashMap[String, Cell]()),
                      Grid(HashMap[String, Cell]()),
                      List[Boat]())
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
            userInput match {
                case "E" =>
                    // create easy AI
                    val newAI = AIEasy(
                      "AIEasy",
                      2,
                      Grid(HashMap[String, Cell]()),
                      Grid(HashMap[String, Cell]()),
                      List[Boat]())
                    mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random)
                case "M" =>
                    // create medium AI
                    val newAI = AIMedium(
                      "AIMedium",
                      2,
                      Grid(HashMap[String, Cell]()),
                      Grid(HashMap[String, Cell]()),
                      List[Boat]())
                    mainLoop(gameState.copy(gameProgress = "1", playerTwo = Some(newAI)), random)
                case "H" =>
                    // create hard AI
                    val newAI = AIHard(
                      "AIHard",
                      2,
                      Grid(HashMap[String, Cell]()),
                      Grid(HashMap[String, Cell]()),
                      List[Boat]())
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
                    val newPlayerOne: Human = Human.placeBoats(Human(
                      gameState.playerOne.get.name,
                      0,
                      Grid(HashMap[String, Cell]()),
                      Grid(HashMap[String, Cell]()),
                      List[Boat]() ),
                      listBoats,
                      random)
                    val newPlayerTwo: Human = Human.placeBoats(Human(
                      gameState.playerTwo.get.name,
                      1,
                      Grid(HashMap[String, Cell]()),
                      Grid(HashMap[String, Cell]()),
                      List[Boat]() ),
                      listBoats,
                      random)
                    mainLoop(gameState.copy(gameProgress = "2", playerOne= Some(newPlayerOne), playerTwo = Some(newPlayerTwo)), random)
                } else if (gameState.gameMode == 2) {
                    val newPlayerOne: Human = Human.placeBoats(Human( gameState.playerOne.get.name, 0,
                      Grid(HashMap[String, Cell]()),
                      Grid(HashMap[String, Cell]()),
                      List[Boat]() ),
                      listBoats,
                      random)
                    val newAI: Player = Player.placeBoatsRandom( gameState.playerTwo.get, listBoats, random, 0)
                    mainLoop(gameState.copy(gameProgress = "2", playerOne= Some(newPlayerOne), playerTwo = Some(newAI)), random)
                } else {
                    val newAIOne: Player = Player.placeBoatsRandom( gameState.playerOne.get, listBoats, random, 0)
                    val newAITwo: Player = Player.placeBoatsRandom( gameState.playerTwo.get, listBoats, random, 0)
                    mainLoop(gameState.copy(gameProgress = "2", playerOne= Some(newAIOne), playerTwo = Some(newAITwo)), random)
                }
            // Playing game
            case "2" =>
                gameState.gameMode match {
                    case 1 | 2 =>
                        gameState.currentPlayer match {
                            case 0 => val (newPlayerOne, newPlayerTwo): (Player, Player) = gameState.playerOne.get.shoot(gameState.playerTwo.get, random)
                                if ( newPlayerTwo.checkGameLost() ) mainLoop(gameState.copy(playerOne = Some(newPlayerOne), playerTwo = Some(newPlayerTwo), gameProgress = "3"), random)
                                else mainLoop(gameState.copy(playerOne = Some(newPlayerOne), playerTwo = Some(newPlayerTwo), currentPlayer = 1), random)
                            case 1 => val (newPlayerTwo, newPlayerOne): (Player, Player) = gameState.playerTwo.get.shoot(gameState.playerOne.get, random)
                                if ( newPlayerOne.checkGameLost() ) mainLoop(gameState.copy(playerOne = Some(newPlayerOne), playerTwo = Some(newPlayerTwo), gameProgress = "3"), random)
                                else mainLoop(gameState.copy(playerOne = Some(newPlayerOne), playerTwo = Some(newPlayerTwo), currentPlayer = 0), random)
                        }
                    case 3 =>
                        playGameAI(gameState, random, 100)
                }

            // End Game
            case "3" => gameState.currentPlayer match {
                case 0 => printGameOver(gameState.playerOne.get)
                case 1 => printGameOver(gameState.playerTwo.get)
                }
                showPrompt(
                    "Do you wish to play again ? \n"
                    + "(y)es or (n)o\n")
                val userInput = getUserInput("remakeGame").getOrElse("wrongInput")
                userInput match {
                case "Y" =>
                    val newFirst = if (gameState.firstPlayer == 0 ) 1 else 0
                    val emptyGrid: Grid =  Grid (HashMap[String, Cell]())
                    val emptyBoats: List[Boat] = List[Boat]()
                    val newPlayerOne = gameState.playerOne.get.update(emptyGrid, emptyGrid, emptyBoats)
                    val newPlayerTwo = gameState.playerTwo.get.update(emptyGrid, emptyGrid, emptyBoats)
                    mainLoop(
                      gameState.copy(
                        firstPlayer = newFirst,
                        currentPlayer = newFirst,
                        playerOne = Some(newPlayerOne),
                        playerTwo = Some(newPlayerTwo),
                        gameProgress = "1"),
                      random)
                case "wrongInput" => mainLoop(gameState, random)
                case _ => showPrompt("Exiting Battleship.\n")
                }
            // default behavior
            case _ => showPrompt(
                "There was an error. \n"
                + "relaunching the game \n")
                mainLoop(s, random)

        }

    }

    /** playGameAI, the function used for the AI proof
    * @param gameState
    * @param random the random generator
    * @param numberGameToPlay the number of games for the AI to play
    */
    def playGameAI(gameState: GameState, random: Random, numberGameToPlay: Int): Unit = {

        /** playTailLoop, the function used to make AI battle one another a number of times
        * @param gameState
        * @param random the random generator
        * @param numberGameToPlay the number of games for the AI to play
        * @param score1 the score of the first player
        * @param score2 the score of the second player
        * @return two Int, the score of both players after all the games have been played
        */
        @tailrec
        def playTailLoop(gameState: GameState, random: Random, numberGameToPlay: Int, score1: Int, score2: Int): (Int, Int) = {
            if (numberGameToPlay == 0) (score1, score2)
            else {
                gameState.currentPlayer match {
                    case 0 => val (newPlayerOne, newPlayerTwo): (Player, Player) = gameState.playerOne.get.shoot(gameState.playerTwo.get, random)
                        if ( newPlayerTwo.checkGameLost() ) {
                            val (newGameState, newRandom): (GameState, Random) = prepareNewgame(gameState, random)
                            playTailLoop(newGameState, newRandom, numberGameToPlay-1, score1+1, score2)
                        }
                        else playTailLoop(
                          gameState.copy(
                            playerOne = Some(newPlayerOne),
                            playerTwo = Some(newPlayerTwo),
                            currentPlayer = 1),
                          random,
                          numberGameToPlay,
                          score1,
                          score2)
                    case 1 => val (newPlayerTwo, newPlayerOne): (Player, Player) = gameState.playerTwo.get.shoot(gameState.playerOne.get, random)
                        if ( newPlayerOne.checkGameLost() ) {
                            val (newGameState, newRandom): (GameState, Random) = prepareNewgame(gameState, random)
                            playTailLoop(newGameState, newRandom, numberGameToPlay-1, score1, score2+1)
                        }
                        else playTailLoop(
                          gameState.copy(
                            playerOne = Some(newPlayerOne),
                            playerTwo = Some(newPlayerTwo),
                            currentPlayer = 0),
                          random,
                          numberGameToPlay,
                          score1,
                          score2)
                }
            }
        }

        /** prepareNewgame, prepare a new game for the two AI, asking them to replace their boats and clearing their grids
        * @param gameState
        * @param random the random generator
        * @return a new gameState, and random.
        */
        def prepareNewgame(gameState: GameState, random: Random): (GameState, Random) = {
            val listBoats: List[Boat] = List(Boat("carrier", 5, List[Cell]()), Boat("battleship", 4, List[Cell]()), Boat("cruiser", 3, List[Cell]()), Boat("submarine", 3, List[Cell]()), Boat("destroyer", 2, List[Cell]()))
            val newAIOne: Player = Player.placeBoatsRandom(
                gameState.playerOne.get.update(
                    ownedGrid = Grid(HashMap[String,Cell]()),
                    opponentGrid = Grid(HashMap[String,Cell]()),
                    boats = List[Boat]()
                ), listBoats, random, 0)
            val newAITwo: Player = Player.placeBoatsRandom(
                gameState.playerTwo.get.update(
                    ownedGrid = Grid(HashMap[String,Cell]()),
                    opponentGrid = Grid(HashMap[String,Cell]()),
                    boats = List[Boat]()
                ), listBoats, random, 0)
            val newFirst = if (gameState.firstPlayer == 0 ) 1 else 0
            ( gameState.copy(playerOne= Some(newAIOne), playerTwo = Some(newAITwo), firstPlayer = newFirst), random )
        }

        val (score1, score2): (Int, Int) = playTailLoop(gameState, random, numberGameToPlay, 0, 0)
        WriteToFile.writeToFile("./ai_proof.csv", gameState.playerOne.get.name + "; " + score1.toString + "; " + gameState.playerTwo.get.name + "; " + score2.toString)
        endGameAI(gameState.playerOne.get.name, score1, gameState.playerTwo.get.name, score2)

    }

    /** endGameAI, shows to the user in the terminal, the result of the battles
    * @param ai1 the first AI
    * @param score1 the score of the first player
    * @param ai2 the second AI
    * @param score2 the score of the second player
    */
    def endGameAI(ai1: String, score1: Int, ai2: String, score2: Int): Unit = {
        if (score1 > score2) showPrompt(ai1 + " a gagné avec un score de " + score1.toString + " à " + score2.toString)
        else if (score1 == score2) showPrompt("Egalité entre " + ai1 + " et " + ai2)
        else showPrompt(ai2 + " a gagné avec un score de " + score2.toString + " à " + score1.toString)
    }
}
