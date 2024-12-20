//import statements
import scalafx.application.JFXApp
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{GridPane, StackPane}
import scalafx.scene.text.Text
import scalafx.scene.text.Font
import scalafx.scene.paint.Color
import scalafx.geometry.Pos
import scalafx.geometry.Pos.Center




class Board { //Defines the chessboard
  var currentPlayer: String = "white" // Initializing current player as white
  //The Board has chess pieces
  val pieces: Array[Piece] = Array()

  //Represent the board as a grid
  private val grid: Array[Array[Option[Piece]]] = Array.fill(8, 8)(None) //Represents the chessboard as a 8x8 grid

  //Sets the position of all the pieces on the board : King ,Queen ,Knight,Rook,Bishop,Pawn
  def setPiecePosition(): Unit = {
    //Placement of the pawns
    for (col <- 0 until 8) { //Iterates through column 0 until column 7 of the grid
      grid(1)(col) = Some(new Pawn("black", (1, col))) //places a black pawn on each cell in row 1
      grid(6)(col) = Some(new Pawn("white", (6, col))) //places a white pawn on each cell in row 6
    }

    //Placement of the Rooks
    grid(0)(0) = Some(new Rook("black", (0, 0))) //places a  black rook at position 0,0 on the grid .The Some keyword is used to place the piece in the cell
    grid(0)(7) = Some(new Rook("black", (0, 7))) //places a black rook at position 0,7 on the grid
    grid(7)(0) = Some(new Rook("white", (7, 0))) //places a white rook at position 7,0 on the grid
    grid(7)(7) = Some(new Rook("white", (7, 7))) //places a white rook at position 7,7 on the grid

    //Placement of the bishops
    grid(0)(2) = Some(new Bishop("black", (0, 2))) //places a  black bishop at position 0,2 on the grid
    grid(0)(5) = Some(new Bishop("black", (0, 5))) //places a  black bishop at position 0,5 on the grid
    grid(7)(2) = Some(new Bishop("white", (7, 2))) //places a white bishop at position 7,2 on the grid
    grid(7)(5) = Some(new Bishop("white", (7, 5))) //places a white bishop at position 7,5 on the grid

    //Placement of the Knights
    grid(0)(1) = Some(new Knight("black", (0, 1))) //places a black knight at position 0,1 on the grid
    grid(0)(6) = Some(new Knight("black", (0, 6))) //places a black knight at position 0,6 on the grid
    grid(7)(1) = Some(new Knight("white", (7, 1))) //places a white knight at position 7,1 on the grid
    grid(7)(6) = Some(new Knight("white", (7, 6))) //places a white knight at position 7,6 on the grid

    //Placement of the Queen
    grid(0)(3) = Some(new Queen("black", (0, 3))) //Places a black queen at position 0,3 on the grid
    grid(7)(3) = Some(new Queen("white", (7, 3))) //Places a white queen at position 7,3 on the grid

    //Placement of the King
    grid(0)(4) = Some(new King("black", (0, 4))) //Places a new black King at position 0,4 on the grid
    grid(7)(4) = Some(new King("white", (7, 4))) //Places a new white King at position 7,4 on the grid
  }


  //defines the movePiece function to move pieces on the board.
  def movePiece(from: (Int, Int), to: (Int, Int)): Boolean = {  //from : (Int,Int) represents the starting position of the piece.  to:(Int,Int) represents the target position of the piece
    grid(from._1)(from._2) match {  //Checks whether there is a piece at the from position on the grid
      case Some(piece) if piece.validMoves(this).contains(to) => //If there is a piece at the from position and the to position is in the list of valid moves for that piece, the move is considered valid
        val capturedPiece = grid(to._1)(to._2) //captured piece is saved to the variable "capturedPiece"
        grid(to._1)(to._2) = Some(piece) //the piece is moved to the to position
        grid(from._1)(from._2) = None //the from position is cleared as there is no piece there
        piece.position = to//"The piece's position is updated to reflect its new location at the to position. (Capturing the opponent's piece, if present, is handled separately.)"

        //Check if the move puts the current player in check
        if (isInCheck(currentPlayer)) { //Checks whether the current player is in check
          // If in check,the moves is undone.Basically the piece goes back to its original from position
          grid(from._1)(from._2) = Some(piece) //Piece goes back to from position
          grid(to._1)(to._2) = capturedPiece//captured piece goes to to position
          piece.position = from//Original Piece goes back to its original position.no capturing of opponents pieces
          false
        } else {
          // Switch the current player after a successful move
          currentPlayer = if (currentPlayer == "white") "black" else "white"
          true
        }
      case _ => false // Invalid move as no piece or move is valid .
    }
  }

  // Basic AI Move for the opponent
  def aiMove(): Boolean = {
    val aiPieces = grid.flatten.flatten.filter(_.color == "black") // flatten the 2D grid into a single list of cells.After that ,filter is applied to the list to only include black pieces which are the AI pieces
    val randomPiece = aiPieces(scala.util.Random.nextInt(aiPieces.length)) // Random ai piece is selected from all the ai pieces (which is the black pieces)

    val validMoves = randomPiece.validMoves(this) // Get all the valid moves for the selected piece
    if (validMoves.nonEmpty) { //checks if the valid moves is not empty .If the valid moves is not empty,then it proceeds with the following lines of code .
      val randomMove = validMoves(scala.util.Random.nextInt(validMoves.length)) // The variable randomMove stores  the randomly selected valid move
      movePiece(randomPiece.position, randomMove) // Performs the movement of the selected piece to the target position .
    } else {
      false //If valid= moves is empty ,the AI does nothing and the function returns false
    }
  }

  //The function isInCheck is created to check whether the king is in checked by the opponents piece
  def isInCheck(player: String): Boolean = {
    val kingPosition = findKingPosition(player) //Finds the king position for the given player
    val opponentPieces = grid.flatten.flatten.filter(_.color != player)//flattens the 2D grid into a single list of all pieces on the board.Fliter out all the pieces that are not the players.basically the opponents,and store it in the variable called opponent pieces

    // Check if any opponent piece can attack the king's position
    opponentPieces.exists(piece => piece.validMoves(this).contains(kingPosition)) //Checks whether any of the opponent pieces can attack the kings position.If any of the opponent piece can attack ,the function returns true ,otherwise it returns  false.
  }


  def isCheckmate(player: String): Boolean = {
    if (!isInCheck(player)) return false // If the players King,not in check ,then the checkmate function returns false.because if there is no check,then there is definitely no checkmate.

    // Check if any move can get the player out of check
    val playerPieces = grid.flatten.flatten.filter(_.color == player) //Gets all the player pieces and stores in the variable called playerPieces .
    playerPieces.exists(piece =>//Loops through each of the players pieces.
      piece.validMoves(this).exists { move =>//For each piece loops through its valid moves
        val from = piece.position//Saves the current position of the piece
        val capturedPiece = grid(move._1)(move._2)//Stores any piece that might be captured at the target position.
        grid(move._1)(move._2) = Some(piece)// Move the piece to the target position.
        grid(from._1)(from._2) = None // Clear the original position of the piece.
        piece.position = move// Update the piece's position to the new location.

        val inCheck = isInCheck(player)// Check if the player is still in check after the move.

        // Undo the move
        grid(from._1)(from._2) = Some(piece)// Put the piece back to its original position.
        grid(move._1)(move._2) = capturedPiece// Restore the captured piece (if any) at the target position.
        piece.position = from// Update the piece's position back to its original position.

        // If the player is not in check after this move, return false (the player can escape check)
        !inCheck
      }
    )
  }


  //The code below is to find the current position of the King
  private def findKingPosition(player: String): Option[(Int, Int)] = {
    grid.flatten.flatten
      .find(p => p.isInstanceOf[King] && p.color == player)
      .map(_.position)
  }



  // Abstract class for Pieces
  abstract class Piece(val color: String, var position: (Int, Int)) {
    def validMoves(board: Board): List[(Int, Int)] //Determines tha valid moves of each piece.Each piece have different valid moves
    def move(to: (Int, Int), board: Board): Unit = {
      if (validMoves(board).contains(to)) {
        position = to
      } else {
        throw new IllegalArgumentException("Invalid move!")
      }
    }
  }
}
class King(color: String, position: (Int, Int)) extends Piece(color, position) { //since the class have unimplemented methods ,so keep it abstract,but make sure later once got the methods inside ,then change it back to normal class,for all the chess pieces)
  
}


class Queen(color: String, position: (Int, Int)) extends Piece(color, position) {
  
}


class Bishop(color: String, position: (Int, Int)) extends Piece(color, position) {
}

class Rook(color: String, position: (Int, Int)) extends Piece(color, position) {
  
}

class Knight(color: String, position: (Int, Int)) extends Piece(color, position) {
  
}

class Pawn(color: String, position: (Int, Int)) extends Piece(color, position) {
  
}


class Player(name: String, score: Int, color: String) {
}


/*class HumanPlayer() extends Player() {

}

class AiPlayer() extends Player() {

}
*/


class Game {

}


// GUI Application with ScalaFX
object ChessApp extends JFXApp3 {

  override def start(): Unit = {
    val grid = new GridPane() {
      prefWidth = 600
      prefHeight = 600
      style = "-fx-background-color:white;"
      alignment = Pos.Center // Align the grid at the centeralignment = Pos.Center // Align the grid at the center
      //Change the alignment of the grid to make it at the centre
    }
    for (row <- 0 until 8) {
      for (col <- 0 until 8) {
        val square = new Button() {
          text = ""
          prefWidth = 50 // Set the width for each square
          prefHeight = 50 // Set the height for each square
          // Alternate the colors of the squares
          style = if ((row + col) % 2 == 0) {
            "-fx-background-color: lightpink;" // Light color square
          } else {
            "-fx-background-color: darkorange;" // Dark color square
          }
        }


        // Create a Text object and add the appropriate piece symbol
        val pieceText = new Text() {
          text = "" // Default empty text
          style = "-fx-fill: black; " // You can set a color for the text
        }

        if (row == 0 || row == 7) { // For both row 0 (white pieces) and row 7 (black pieces)
          col match {
            case 0 | 7 => pieceText.text = if (row == 0) "♖" else "♜" //Rooks
            case 1 | 6 => pieceText.text = if (row == 0) "♘" else "♞" // Knights
            case 2 | 5 => pieceText.text = if (row == 0) "♗" else "♝" // Bishops
            case 3 => pieceText.text = if (row == 0) "♕" else "♛" // Queen
            case 4 => pieceText.text = if (row == 0) "♔" else "♚" // King
            case _ => pieceText.text = "" // Empty squares
          }
        } else if (row == 1 || row == 6) { // For pawns in rows 1 (white pawns) and 6 (black pawns)
          pieceText.text = if (row == 1) "♙" else "♟" // Pawns
        }


        // Add the piece text to the square button
        square.text = pieceText.text.value // Update the button's text with the piece

        grid.add(square, col, row) // Add the square button to the GridPane
      }
    }

    // Create a StackPane to center the grid inside the scene
    val stackPane = new StackPane() {
      children = grid
    }


    stage = new PrimaryStage() {
      title = "Chess Game"
      scene = new Scene(600, 600) { //Creates a new Scene in the GUI application
        root = stackPane
      }
    }
  }
}
