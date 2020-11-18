package board

class Square(val letter: Char, val number: Int) {

  override def toString: String = letter.toString + number

  var hit: Boolean = false

  var containsShip = false

  def placeShip = containsShip = true

  def shoot = {
    if (containsShip) println("Hit!")
    else println("Miss!")
    hit = true
  }
  def isInline(otherSquare: Square): Boolean = {
    if (this.letter.equals(otherSquare.letter) || this.number == otherSquare.number) true
    else false
  }

}

object Square {

  val alphabet = Array('A','B','C','D','E','F','G','H','I','J','K','L')
  def isValidDesignation(square: String): Boolean = {
    if (square.length != 2 && square.length != 3) false
    else if (!alphabet.contains(square.charAt(0))) false
    else if (!(square.tail.toInt >= 1)) false
    else true
  }
}
