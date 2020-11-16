package board

class Ship(val length: Int){

  override def toString: String = s"This ship is $length squares long"

  val squaresOccupied = new Array[Square](length)

  var sunk: Boolean = false

  var hits = 0

  def hit = hits += 1

  def sink = {
    sunk = true
    println("You sunk my battleship")
  }

  def addSquare(sNum: Int, square: Square) = squaresOccupied(sNum) = square

  def printSquares = squaresOccupied.foreach(sq => println(sq.toString))

}
