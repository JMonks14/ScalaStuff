package board

class Board(val size: Int) {

  val alphabet = Array('A','B','C','D','E','F','G','H','I','J')

  val squareList = {
      val squares = new Array[Square](size * size)
      def fillArray(acc1: Int, acc2: Int, acc3: Int): Unit = {
        if (acc3 >= (size * size)) {}
        else if (acc2 > size) fillArray(acc1 + 1, 1, acc3)
        else {
          squares(acc3) = new Square(alphabet(acc1), acc2)
          fillArray(acc1, acc2 + 1, acc3 + 1)
        }
      }
      fillArray(0, 1, 0)
    squares

  }

  val ships = Array(new Ship(2), new Ship(2))

  def locateShips = ships.foreach(sh => {
    println(s"Ship ${ships.indexOf(sh) + 1}:")
    sh.printSquares
  })

  def listSquares = squareList.foreach(s => println(s.toString))

  def chooseSquare(sNum: Int, index: Int, ship: Int) = {
    if (squareList(index).containsShip) false
    else {
      squareList(index).placeShip
      ships(ship).addSquare(sNum,squareList(index))
      true
    }

  }

  def shoot(square: String): String = {
    var ret = "miss"
    val target = squareList.filter(s => s.toString.equals(square)).head
    if (target.hit) {
      println("You have already shot at that square")
      ret = "fail"
    }
    else {
      target.shoot
      ships.foreach(ship =>
          ship.squaresOccupied.foreach(s => {
            if (s.toString.equals(square)) {
              ship.hit
              ret = "hit"
              if (ship.hits >= ship.length) ship.sink
            }
          })
        )
    }
    ret
  }

  def showShots = {
    squareList.foreach(sq => if (sq.hit) {
      if (sq.containsShip) println(sq.toString + ": Hit")
      else println(sq.toString + ": Miss")
    })
  }

  def isValidDist(length: Int, frontIndex: Int, backIndex: Int): Boolean = {

    val diff = Math.abs(frontIndex-backIndex)
    if (length - 1 == diff || diff == this.size * (length - 1)) true
    else false

  }

}
