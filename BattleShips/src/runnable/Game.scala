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

  println("Player 1: Where would you like to place your third ship?")

  placeShip(board,2)

  println("Player 2: Where would you like to place your first ship?")

  placeShip(board2,0)

  println("Player 2: Where would you like to place your second ship?")

  placeShip(board2,1)

  println("Player 2: Where would you like to place your third ship?")

  placeShip(board2,2)

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

//  def getSecondSquareIndex(board: Board, frontSquareIndex: Int, square: (Char, Int), ship: Ship): Int = {
//    val frontSquare = board.squareList(frontSquareIndex)
//    val index = Square.alphabet.indexOf(square._1) * boardsize + square._2 - 1
//    val backSquare = board.squareList(index)
//
//    if (frontSquare.isInline(backSquare) && board.isValidDist(ship.length, frontSquareIndex, index)) index
//    else {
//      println("That square is unavailable for the size and position of this ship, please try again")
//      getSecondSquareIndex(board, frontSquareIndex, getSquare, ship)
//    }
//  }

  def getTarget: String = {
    val tScanner = new Scanner(System.in)
    val input = tScanner.nextLine.toUpperCase
    if (Square.isValidDesignation(input) && input(1).asDigit <= boardsize) input
    else {
      println("Invalid input, please try again")
      getTarget
    }
  }

  def shoot(board: Board, target: String, acc: Int): Unit = {
    val shot = board.shoot(target)
    if (acc == 0) {
    shot match {
      case "hit" => {
        if (!(board.ships.count(ship => ship.sunk) >= board.ships.length)) {
          println("Shoot again!")
          shoot(board, getTarget, acc + 1)
        }
      }
      case "fail" =>  {
        println("please specify a new target")
        shoot(board, getTarget, acc)
      }
      case "miss" =>
    }
  } else {
      shot match {
        case "hit" =>
        case "miss" =>
        case "fail" => {
          println("please specify a new target")
          shoot(board, getTarget, acc + 1)
        }
      }
    }
  }

  def placeShip(board: Board,shipNo: Int) = {

    def pickSquare1(sNum: Int, index: Int): Int = {
      if (board.squareList(index).containsShip) {
        println("This square is already occupied, please choose another")
        pickSquare1(sNum, getSquareIndex(getSquare))
      }
      else {
        val possibles = board.dispPossiblePlaces(index, board.ships(shipNo).length)
        if (possibles.isEmpty) {
          println("There is not enough space here to fit a ship, please choose another space")
          pickSquare1(sNum, getSquareIndex(getSquare))
        } else {
          index
        }
      }
    }
    println("Please choose a square for the front of your ship")
    val frontIndex = pickSquare1(0, getSquareIndex(getSquare))
    println("Please choose a square for the back of your ship, your possible choices are:")
    val possibles = board.dispPossiblePlaces(frontIndex, board.ships(shipNo).length)
    possibles.foreach(sq => println(sq.toString))

    def getBackIndex(index: Int): Int = {
      if (possibles contains board.squareList(index)) index
      else {
        println("That square is not a viable option for this ship")
        getBackIndex(getSquareIndex(getSquare))
      }
    }
    val backIndex = getBackIndex(getSquareIndex(getSquare))

    val allIndexes = {
      board.getAllSquaresInLine(board.ships(shipNo).length,board.squareList(frontIndex), board.squareList(backIndex)).map(sq => board.squareList.indexOf(sq))
    }

    allIndexes.foreach(i => board.chooseSquare(allIndexes.indexOf(i),i, shipNo))
  }

  def playTillSunk(acc: Int): Unit = {
    val shipsSunk1 = board.ships.count(ship => ship.sunk)
    val shipsSunk2 = board2.ships.count(ship => ship.sunk)
    if (shipsSunk1 >= board.ships.length) println("Game Over, Player 2 wins!")
    else if (shipsSunk2 >= board.ships.length) println("Game Over, Player 1 wins!")
    else if (acc % 2 == 1){
      println("")
      println(s"Turn ${(acc+1)/2}: Player 1")
      if (acc > 1) {
        println("Your shots so far:")
        board2.showShots
      }
      println("Choose a target")
      shoot(board2, getTarget, 0)
      playTillSunk(acc + 1)
    } else {
      println("")
      println(s"Turn ${(acc+1)/2}: Player 2")
      if (acc > 1) {
        println("Your shots so far:")
        board.showShots
      }
      println("Choose a target")
      shoot(board, getTarget, 0)
      playTillSunk(acc + 1)
    }
  }
}





