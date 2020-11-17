package runnable

import board.{Board, Square}


object Tests extends App {

  val board = new Board(3)

//  board.listSquares

//  board.getAllSquaresInLine(3, board.squareList(0), board.squareList(8)).foreach(sq => println(sq.toString))

//  board.getAllSquaresInLine(4, board.squareList(13), board.squareList(1)).foreach(sq => println(sq.toString))

println(board.isBackViable(2,board.squareList(0),board.squareList(1)))
val possibles = board.dispPossiblePlaces(1, 2)
  possibles.foreach(sq => println(sq.toString))
}
