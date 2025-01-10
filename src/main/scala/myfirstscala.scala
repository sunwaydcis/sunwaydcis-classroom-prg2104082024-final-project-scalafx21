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

//Defining the class Board
class Board {
  val grid: Array[Array[Option[Piece]]] = Array.fill(8, 8)(None) //Board has a grid.the grid is a 8x8 empty grid with no pieces .
  val validMoveGrid : Array[Array[String]] = Array.fill(8,8)(".")//A separate grid for marking valid moves.

  var currentPlayer: String = "white" // Initializing current player.The player that starts first is white, so thats why current player is initialized to white.]f  f] f]]]]

  //Method that initializes all the pieces on their respective positions on the chessboard.
  def initializeBoard(): Unit = {
    //Placement of the Rooks
    grid(0)(0) = Some(new Rook("black",false,Position(0,0)))
    grid(0)(7) = Some(new Rook("black",false,Position(0,7)))
    grid(7)(0) = Some(new Rook("white",true,Position(7,0)))
    grid(7)(7) = Some(new Rook("white",true,Position(7,7)))

    //Placement of the Knights
    grid(0)(1) = Some(new Knight("black",false,Position(0,1)))
    grid(0)(6) = Some(new Knight("black",false,Position(0,6)))
    grid(7)(1) = Some(new Knight("white",true,Position(7,1)))
    grid(7)(6) = Some(new Knight("white",true,Position(7,6)))

    //Placement of the bishops
    grid(0)(2) = Some(new Bishop("black",false,Position(0,2)))
    grid(0)(5) = Some(new Bishop("black",false,Position(0,5)))
    grid(7)(2) = Some(new Bishop("white",true,Position(7,2)))
    grid(7)(5) = Some(new Bishop("white",true,Position(7,5)))

    //Placement of the Queens
    grid(0)(3) = Some(new Queen("black",false,Position(0,3)))
    grid(7)(3) = Some(new Queen("white",true,Position(7,3)))

    //Placement of the Kings
    grid(0)(4) = Some(new King("black",false,Position(0,4)))
    grid(7)(4) = Some(new King("white",true,Position(7,4)))

    //Placement of the Pawns
    for (col <- 0 to 7) {
      // Black pawns placed on row 1 (index 1)
      grid(1)(col) = Some(new Pawn("black", false, Position(1, col)))

      // White pawns placed on row 6 (index 6)
      grid(6)(col) = Some(new Pawn("white", true, Position(6, col)))
    }
  }

  //Method to get the piece at the specific position in the grid
  def getPieceAt(position: Position): Option[Piece] = {
    if (position.row >= 0 && position.row < 8 && position.col >= 0 && position.col < 8) {
      grid(position.row)(position.col) // Return the piece at the specified position
    } else {
      println(s"Invalid position: (${position.row}, ${position.col})") // Print an error message for invalid position
      None // Return None for out-of-bounds positions
    }
  }

  //method that allows players to see all the valid moves that the piece can make.
  def displayBoard():Unit={
    for(row <- grid.indices) {
      for (col <- grid(row).indices) {
        print(validMoveGrid(row)(col) + " ") // Print valid move grid instead of pieces
      }
      println()
    }
  }

  //Method that allows the chess pieces to move on the board
  def makeMove(from: (Int, Int), to: (Int, Int)): Unit = { //accepts the start and end positions as parameters
    val piece = grid(from._1)(from._2) // gets the piece at each position on the grid 
    if (piece.isEmpty) { // Check if there's no piece at the starting position.
      println("Invalid move: No piece at the starting position.") //Prints an error message that nom piece is found.
      return
    }
    if ((currentPlayer == "white" && !piece.get.isWhite) || (currentPlayer == "black" && piece.get.isWhite)) { //Condition to ensure that players can only move when it is their turn.
      println(s"Invalid move: It's $currentPlayer's turn.")
      return
    }
    // Check if the destination is occupied by a piece of the same color
    val targetPiece = grid(to._1)(to._2) //gets the piece at the specified position
    if (targetPiece.isDefined && targetPiece.get.isWhite == piece.get.isWhite) { //Checks whether there is a piece there and whether it is the same colour as the piece being moved).
      println("Invalid move: Cannot move to a square occupied by your own piece.") //Prints an error message that the move is invalid because the square is occupied by the piece of the same colour.
      return
    }

    // Check if the move is valid according to the piece's movement rules
    if (!piece.get.isValidMove(Position(from._1, from._2), Position(to._1, to._2), this)) {
      println("Invalid move: The move is not valid for this piece.")
      return
    }

  }

  //method that allows players to switch turns alternatively .
  def switchPlayer(): Unit = { //method to allow players to switch turns.
    currentPlayer = if (currentPlayer == "white") "black" else "white" //If the current player is white then the currentPlayer switches to black,else it switches back to white.
    println(s"Player switched. It's now $currentPlayer's turn.") //A message is printed that the player has switched .It is now current player's turn(black)
  }


  //Method to check if King is in check.
  def isKingCheck(board: Board,currentPlayer:Player) : Unit = { //Takes the parameter of the board and the Player
    //Logic below.
    // Step 1 : get the current players King position.
    //Step 2 : Check the position of all the opponents.
    //Step 3 : See whether any of the opponents piece is able to attack the king and able to check it.
  }

  //method to check if the King is in checkmate.
  def isCheckmate(): Unit = { //method to check whether the king is in checkmate(crucial logic).
    //Logic : If statement to check whether the current player's king is in checkmate.meaning there are no other moves possible.
    println("Your King is in checkmate! Game Over") // A message is printed that King is in checkmate and the game is over
  }
}



//defining the case class Position
case class Position(row: Int, col: Int) { //The class Position takes in the two parameters of row index  and column index

  //method to check whether the position is valid.
  def isValid: Boolean = { //validity checking : Check to see whether the position is valid (within the chessBounds)
    row >= 0 && row < 8 && col >= 0 && col < 8 //row must be between 0 and 7. Column must be between 0 and 7.returns true if position is valid.returns false otherwise.
  }

  def distanceTo(other: Position): Int = { //method to calculate the distance between 2 points
    Math.abs(row - other.row) + Math.abs(col - other.col) //Takes the original position row index and column index minus with the other positions row index and column index and then plus them both together to get the distance between the 2 points.
  }

  override def equals(obj: Any): Boolean = { //method to check whether the 2 positions are equal
    obj match {
      case p: Position => p.row == row && p.col == col //case where 2 positions are equal.return true.
      case _ => false //case where 2 positions are not equal.return false.
    }
  }
}

//  Piece class
abstract class Piece(val color: String, val isWhite: Boolean, var position: Position) {
  //The abstract class Piece represents a chess piece with :
  //-color:The color of the piece (immutable, e.g., "white" or "black")
  //-position:The current position of the piece on the board (mutable)
  //-isWhite : Indicates if the piece is white (derived from the color)
  def isValidMove(from: Position, to: Position, board: Board): Boolean // method to check if a move from one position to another is valid.

  def getValidMoves(board: Board): List[Position] = { //method to calculate all the valid moves of the piece from its current position.
    val boardSize = 8
    val allPossibleMoves = for {
      row <- 0 until boardSize
      col <- 0 until boardSize
      to = Position(row, col)
      if isValidMove(position, to, board)  // Only keep valid moves
    } yield to
      allPossibleMoves.toList
  }


  def onClick(board: Board): Unit = { //This method is executed when a piece is clicked.
    val validMoves = getValidMoves(board) //stores the validMoves in a validMoves variable.

    // Prints the valid moves of the piece, along with its current position.
    println(s"Valid moves for ${this.display()} at ${position.row}, ${position.col}:")
    validMoves.foreach(move => println(s"(${move.row}, ${move.col})"))
  }

 // def isKing: Boolean = this.isInstanceOf[King] // Method to check if the piece is a King

  def display(): String //method to display the piece as a string representation.

  def move(to: Position,board: Board): Unit = { //method to move piece to new Position
    if(isValidMove(position,to,board)){
      position = to
      println(s"Moving piece to ${to.row}, ${to.col}")
    }else{
      println(s"Invalid move to ${to.row}, ${to.col}")
    }

  }
}

//  King class
class King(color: String, isWhite: Boolean, position: Position) extends Piece(color, isWhite, position) with Movable with Displayable { //King class is a subclass of the Piece class.
  override def isValidMove(from: Position, to: Position, board: Board): Boolean = {
    val rowDiff = Math.abs(from.row - to.row) //Calculates the difference between the from position row index and the to position row index.
    val colDiff = Math.abs(from.col - to.col) //Calculates the difference between the from position column index and the to position column index.

    // A king can move one square in any direction (vertically, horizontally, or diagonally)
    rowDiff <= 1 && colDiff <= 1 //King can only move 1 square in any direction ,therefore the difference must be equal to 1 or less .
  }

  override def display(): String = {//display the King on the board
    if (isWhite) "\u2654" // White King
    else "\u265A" // Black King
  }

  override def onClick(board: Board): Unit={ //method to define what will happen when the king piece is clicked
    //Step 1 :get the valid moves of the King
    val validMoves = getValidMoves(board)
    //Step 2 :Represent the valid moves of the King using a dot.
    validMoves.foreach { move =>
      board.validMoveGrid(move.row)(move.col) = "•"
    }
    // Step 3 :Ensure board update.
    board.displayBoard()
  }
}

// Queen class
class Queen(color: String, isWhite: Boolean, position: Position) extends Piece(color, isWhite, position) with Movable with Displayable { //Queen class is a subclass of the Piece class.
  override def isValidMove(from: Position, to: Position, board: Board): Boolean = {
    val rowDiff = Math.abs(from.row - to.row)//Calculates the difference between the from position row index and the to position row index.
    val colDiff = Math.abs(from.col - to.col)//Calculates the difference between the from position column index and the to position row index.

    (from.row == to.row || from.col == to.col || rowDiff == colDiff) //ensures that the queen can move horizontally,vertically or diagonally any number of squares.
  }

  override def display(): String = {//display the queen on the board
    if(isWhite) "\u2655"
    else "\u265B"
  }

  override def onClick(board: Board): Unit = { //method to define what will happen when the queen piece is clicked

  }
}

// Rook class
class Rook(color: String, isWhite: Boolean, position: Position) extends Piece(color, isWhite, position) with Movable with Displayable {

  // Override isValidMove for Rook
  override def isValidMove(from: Position, to: Position, board: Board): Boolean = {
    val rowDiff = Math.abs(from.row - to.row) //Calculates the difference between the row index of the to position and the row index of the from position.
    val colDiff = Math.abs(from.col - to.col) //Calculate the difference between the column index of the from position and the column index of the to position.

    (from.row == to.row || from.col == to.col) && !isPathBlocked(from, to, board) //Ensures that Rook only moves vertically and horizontally, and the path is not blocked by other pieces.
  }

  // Helper method to check if there is a piece blocking the Rook's path
  def isPathBlocked(from: Position, to: Position, board: Board): Boolean = {
    // Horizontal movement
    if (from.row == to.row) {
      val minCol = Math.min(from.col, to.col)
      val maxCol = Math.max(from.col, to.col)
      for (col <- (minCol + 1) until maxCol) {
        if (board.grid(from.row)(col).isDefined) return true // If blocked by  a piece, then the method returns true.(horizontal movement of the Rook)
      }
    }
    // Vertical movement
    if (from.col == to.col) {
      val minRow = Math.min(from.row, to.row)
      val maxRow = Math.max(from.row, to.row)
      for (row <- (minRow + 1) until maxRow) {
        if (board.grid(row)(from.col).isDefined) return true // If blocked by a piece ,then the method returns true (vertical movement of the Rook)
      }
    }
    false
  }

  override def display(): String = { //displays the Rook on the board
    if (isWhite) "\u2656"
    else "\u265C"
  }

  override def onClick(board: Board): Unit ={ //method to define what will happen when the Rook is clicked

  }
}

// Bishop class
class Bishop(color: String, isWhite: Boolean, position: Position) extends Piece(color, isWhite, position) with Movable with Displayable  {

  // Override isValidMove for Bishop
  override def isValidMove(from: Position, to: Position, board: Board): Boolean = {
    val rowDiff = Math.abs(from.row - to.row)//Calculates the difference between the row index of the to position and the row index of the from position.
    val colDiff = Math.abs(from.col - to.col)//Calculate the difference between the column index of the from position and the column index of the to position.

    // A bishop can move diagonally (rowDiff == colDiff)
    rowDiff == colDiff && !isPathBlocked(from, to, board) //Ensures that bishop can only move diagonally and when path is not blocked by other pieces
  }

  // Helper method to check if there is a piece blocking the Bishop's path
  def isPathBlocked(from: Position, to: Position, board: Board): Boolean = {
    val rowDiff = Math.abs(from.row - to.row)
    val colDiff = Math.abs(from.col - to.col)

    val rowDirection = if (to.row > from.row) 1 else -1
    val colDirection = if (to.col > from.col) 1 else -1

    var currentRow = from.row + rowDirection
    var currentCol = from.col + colDirection

    // Move along the diagonal path and check for any blocking pieces
    for (_ <- 1 until rowDiff) { // We start from 1 because we've already checked the start position
      board.grid(currentRow)(currentCol) match {
        case Some(_) => return true // Blocked by a piece
        case None => // No piece, continue
      }
      currentRow += rowDirection
      currentCol += colDirection
    }
    false // Path is not blocked
  }

  // Override display method to show the Bishop as "B"
  override def display(): String = { //displays the bishop on the board.
    if (isWhite) "\u2657"
    else "\u265D"
  }

  override def onClick(board: Board): Unit = { //method to define what will happen when the bishop  is clicked

  }
}

// Knight class
class Knight(color: String, isWhite: Boolean, position: Position) extends Piece(color, isWhite, position) with Movable with Displayable {
  // Override isValidMove for Knight
  override def isValidMove(from: Position, to: Position, board: Board): Boolean = {
    val rowDiff = Math.abs(from.row - to.row)
    val colDiff = Math.abs(from.col - to.col)

    // A knight moves in an L-shape: two squares in one direction and one square in the other direction.
    (rowDiff == 2 && colDiff == 1) || (rowDiff == 1 && colDiff == 2)
  }

  // Override display method to show the Knight as "N"
  override def display(): String = { //displays the knight on the board
    if (isWhite) "\u2658"
    else "\u265E"
  }

  override def onClick(board: Board): Unit = { //defines what will happen when the Knight is clicked.

  }
}

// Pawn class
class Pawn(color: String, isWhite: Boolean, position: Position) extends Piece(color, isWhite, position) with Movable with Displayable {
  override def isValidMove(from: Position, to: Position, board: Board): Boolean = {
    val rowDiff = Math.abs(from.row - to.row)
    val colDiff = Math.abs(from.col - to.col)

    // Pawns can move one square forward (or two squares on their first move)
    if (from.col == to.col && rowDiff == 1) {
      return true
    }
    if (from.col == to.col && rowDiff == 2 && from.row == (if (isWhite) 1 else 6)) {
      // First move can be two squares forward
      return true
    }
    // Pawns can capture diagonally
    if (colDiff == 1 && rowDiff == 1 && board.getPieceAt(to).isDefined &&
      board.getPieceAt(to).get.isWhite != this.isWhite) {
      return true
    }

    //Pawns cannot move backwards
    if (isWhite && from.row > to.row || !isWhite && from.row < to.row) {
      return false
    }
    false
  }

  override def display(): String = { //displays the pawn on the board
    if (isWhite) "\u2659"
    else "\u265F"
  }

  override def onClick(board: Board): Unit = { //defines what will happen when the pawn is clicked
    val validMoves = getValidMoves(board)

    // Debugging: Print valid moves to ensure they are being calculated
    println(s"Valid moves for pawn at ${position}: ${validMoves.mkString(", ")}")
    //Step 2 :Represent the valid moves of the King using a dot.
    validMoves.foreach { move =>
      board.validMoveGrid(move.row)(move.col) = "•"
    }
    // Step 3 :Ensure board update.
    board.displayBoard()
  }
}

//Player class
class Player(private val color: String) { //Player has a property color.
  def getColor: String = color// Getter method for color,

  def makeMove(board: Board): Unit = {
    println(s"$color's AI is making a move.")

  }
}

class  HumanPlayer(color: String) extends Player(color) { //HumanPlayer is a subclass of Player
  // Human makes a move by selecting a starting and ending position.
  override def makeMove(board: Board): Unit = {
    println(s"$color's turn to move")
    println("Enter starting position (row, column): ")
    val startRow = scala.io.StdIn.readInt()
    val startCol = scala.io.StdIn.readInt()

    println("Enter ending position (row, column): ")
    val endRow = scala.io.StdIn.readInt()
    val endCol = scala.io.StdIn.readInt()

    // Create positions
    val startPos = new Position(startRow, startCol)
    val endPos = new Position(endRow, endCol)

  }
}


class  AiPlayer(color: String) extends Player(color) {
  override def makeMove(board: Board): Unit = {
    println(s"$color's AI is making a move.")
  }

}

//Game class
class Game {
  val board: Board = new Board //Game contains a board object,which represents the chessboard.
  val whitePlayer: Player = new Player("white") //Game has a white player.(represented by the player class)
  val blackPlayer: AiPlayer = new AiPlayer("black") //Game has a black player .(represented by the AI player class)
  var currentPlayer: Player = whitePlayer //Initially ,the current player is white player.

  def startGame(): Unit = { //method to start game
    println("Game started!") //prints a message indicating that the game has started.
  }

  def endGame() :Unit = {
    println("Game ended!") //prints a message indicating that the game has ended.
  }
}

//case class Move that represents a move for piece on the board .
case class Move[T <: Piece](piece: T, from: Position, to: Position) {
  // Method to check if the move is valid
  def isValidMove(board: Board): Boolean = {
    // Get the list of valid moves for the piece
    //val validMoves = piece.getValidMoves(board)
    // Ensure the destination is not occupied by a piece of the same color
    val destinationPiece = board.getPieceAt(to)
    if (destinationPiece.isDefined && destinationPiece.get.color == piece.color) {
      return false // Cannot land on a square occupied by a piece of the same color
    }

    return true // The move is valid
  }

  def applyMove(board: Board): Unit = {
    //Ensure that there is a piece at the starting position
    val startSquare = board.grid(from.row)(from.col)
    if (startSquare.isEmpty) {
      println("No piece at the starting position!")
      return
    }

    if (isValidMove(board)) {
      val piece = startSquare.get
      piece.move(to,board)

      //update the board with the moved piece
      board.grid(from.row)(from.col) = None //Remove the piece from the old position
      board.grid(to.row)(to.col) = Some(piece) //Place the piece at the new position

      //Switch the player turn if applicable
      board.switchPlayer()
    } else {
      println("Invalid Move!")
    }
  }
}


trait  Movable() {
  // Method to validate a move for the piece
  def isValidMove(from: Position, to: Position, board: Board): Boolean
}

trait  Displayable() {
  // Method to get the string representation of a piece
  def display(): String
}

object ChessApp extends JFXApp3 {
  override def start(): Unit = {
    val board = new Board()
    board.initializeBoard()

    val chessboard = new GridPane() {
      prefWidth = 600
      prefHeight = 600
      style = "-fx-background-color:white"
    }

    // Add column and row constraints to ensure the grid looks like a chessboard
    for (_ <- 0 until 8) {
      chessboard.getColumnConstraints.add(new scalafx.scene.layout.ColumnConstraints(75))
      chessboard.getRowConstraints.add(new scalafx.scene.layout.RowConstraints(75))
    }

    for (row <- 0 until 8) {
      for (col <- 0 until 8) {
        val square = new Button() {
          text = ""
          prefWidth = 75
          prefHeight = 75
          style = if ((row + col) % 2 == 0) {
            "-fx-background-color:lightPink; -fx-background-radius: 0; -fx-background-insets: 0; -fx-font-size: 30px; -fx-text-fill: black;"
          } else {
            "-fx-background-color: grey; -fx-background-radius: 0; -fx-background-insets: 0; -fx-font-size: 30px; -fx-text-fill: black;"
          }

          onAction = _ => {
            println(s"Square clicked at row $row, col $col")
            board.grid(row)(col).foreach { piece =>
              println(s"Piece clicked: ${piece.display()}") // Display the clicked piece
              // Add logic for what will happen when piece is clicked
            }
          }
        }

        val piece = board.grid(row)(col)
        piece.foreach { p =>
          square.text = p.display() // Set the text of the square(button to the string representation of the piece.
        }

        chessboard.add(square, col, row)
      }
    }

    // Initialize the scene and stage properly
    val scene2= new Scene {
      fill = Color.Blue
      content = chessboard
    }

    stage = new PrimaryStage {
      title = "AI Chess Game"
      scene = scene2 // Assign the scene to the stage
    }
  }
}
