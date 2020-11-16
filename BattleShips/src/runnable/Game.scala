package runnable

import java.util.{InputMismatchException, Scanner}

import board.{Board, Ship, Square}


object Game extends App {

  println("Hello, welcome to battleships, what size board would you like to play on? (please enter an integer from 3 - 10)")

  val boardsize = getBoardSize

  val board = new Board(boardsize)
  val board2 = new Board(boardsize)

  println("Player 1: Where would you like to place your first ship?")

  placeShip(board,0)

  println("Player 1: Where would you like to place your second ship?")

  placeShip(board,1)

  println("Player 2: Where would you like to place your first ship?")

  placeShip(board2,0)

  println("Player 2: Where would you like to place your second ship?")

  placeShip(board2,1)

  playTillSunk(1)

  def getSquare: (Char, Int) = {
    val sqScanner = new Scanner(System.in)
    val input = sqScanner.nextLine.toUpperCase
    if (Square.isValidDesignation(input) && input(1).asDigit <= boardsize)  (input.charAt(0), input(1).asDigit)
    else {
      println("Invalid input, please try again")
      getSquare
      }
    }

  def getBoardSize: Int = {
    def getInt: Int = {
      try {
        val scanner = new Scanner(System.in)
        scanner.nextInt()
      } catch {
        case e: InputMismatchException => {
          println("You must enter an integer between 3 and 10")
          getInt
        }
      }
    }
    val input = getInt
    if (!(input >= 3 && input <= 10)) {
      println("You must enter an integer between 3 and 10")
      getBoardSize
    } else input

  }

  def getSquareIndex(square: (Char, Int)) = Square.alphabet.indexOf(square._1) * boardsize + square._2 - 1

  def getSecondSquareIndex(board: Board, frontSquareIndex: Int, square: (Char, Int), ship: Ship): Int = {
    val frontSquare = board.squareList(frontSquareIndex)
    val index = Square.alphabet.indexOf(square._1) * boardsize + square._2 - 1
    val backSquare = board.squareList(index)

    if (frontSquare.isInline(backSquare) && board.isValidDist(ship.length, frontSquareIndex, index)) index
    else {
      println("That square is unavailable for the size and position of this ship, please try again")
      getSecondSquareIndex(board, frontSquareIndex, getSquare, ship)
    }

  }

  def getTarget: String = {
    val tScanner = new Scanner(System.in)
    val input = tScanner.nextLine.toUpperCase
    if (Square.isValidDesignation(input) && input(1).asDigit <= boardsize) input
    else {
      println("Invalid input, please try again")
      getTarget
    }
  }

  def shoot(board: Board, target: String): Unit = {
    val shot = board.shoot(target)
    if (shot) {}
    else {
      println("please specify a new target")
      shoot(board, getTarget)
    }
  }

  def placeShip(board: Board,shipNo: Int) = {

    def pickSquare1(sNum: Int, index: Int): Int = {
      val placed = board.chooseSquare(sNum, index, shipNo)
      if (placed) index
      else {
        println("This square is already occupied, please choose another")
        pickSquare1(sNum, getSquareIndex(getSquare))
      }
    }
    println("Please choose a square for the front of your ship")
    val frontIndex = pickSquare1(0, getSquareIndex(getSquare))


    def pickSquare2(sNum:Int, index: Int): Unit = {
      val placed = board.chooseSquare(sNum, index, shipNo)
      if (placed) {}
      else {
        println("This square is already occupied, please choose another")
        pickSquare2(sNum, getSecondSquareIndex(board, frontIndex ,getSquare, board.ships(shipNo)))
      }
    }

    println("Please choose a square for the back of your ship")
    val backIndex = getSecondSquareIndex(board, frontIndex, getSquare, board.ships(shipNo))
    pickSquare2(1, backIndex)

    }

  def playTillSunk(acc: Int): Unit = {
    val shipsSunk1 = board.ships.count(ship => ship.sunk)
    val shipsSunk2 = board2.ships.count(ship => ship.sunk)
    if (shipsSunk1 >= board.ships.length) println("Game Over, Player 2 wins!")
    else if (shipsSunk2 >= board.ships.length) println("Game Over, Player 1 wins!")
    else if (acc % 2 == 1){
      println("")
      println(s"Turn $acc: Player 1")
      if (acc > 1) {
        println("Your shots so far:")
        board.showShots
      }
      println("Choose a target")
      shoot(board, getTarget)
      playTillSunk(acc + 1)
    } else {
      println("")
      println(s"Turn $acc: Player 2")
      if (acc > 1) {
        println("Your shots so far:")
        board2.showShots
      }
      println("Choose a target")
      shoot(board2, getTarget)
      playTillSunk(acc + 1)
    }
  }

  }





