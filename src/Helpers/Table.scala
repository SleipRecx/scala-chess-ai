package Helpers

object Table{

  private val whitePawn = Array(
    Array(1000,0,0,0,0,0,0,0),
    Array(5,10,10,-20,-20,10,10,5),
    Array(5,-5,-10,0,0,-10,-5,5),
    Array(0,0,0,20,20,0,0,0),
    Array(5,5,10,25,25,10,5,5),
    Array(10,10,20,30,30,20,10,10),
    Array(50,50,50,50,50,50,50,50),
    Array(0,0,0,0,0,0,0,0))


  private val whiteKnight = Array(
    Array(-50,-40,-30,-30,-30,-30,-40,-50),
    Array(-40,-20,0,5,5,0,-20,-40),
    Array(-30,5,10,15,15,10,5,-30),
    Array(-30,0,15,20,20,15,0,-30),
    Array(-30,0,15,20,20,15,0,-30),
    Array(-30,5,10,15,15,10,5,-30),
    Array(-40,-20,0,5,5,0,-20,-40),
    Array(-50,-40,-30,-30,-30,-30,-40,-50))

  private val whiteBishop = Array(
    Array(-20,-10,-10,-10,-10,-10,-10,-20),
    Array(-10,5,0,0,0,0,5,-10),
    Array(-10,10,10,10,10,10,10,-10),
    Array(-10,0,10,10,10,15,0,-10),
    Array(-10,5,5,10,10,5,5,-10),
    Array(-10,0,5,10,10,5,0,-10),
    Array(-10,0,0,0,0,0,0,-10),
    Array(-20,-10,-10,-10,-10,-10,-10,-20))

  private val whiteRook = Array(
    Array(0,0,0,5,5,0,0,0),
    Array(-5,0,0,0,0,0,0,-5),
    Array(-5,0,0,0,0,0,0,-5),
    Array(-5,0,0,0,0,0,0,-5),
    Array(-5,0,0,0,0,0,0,-5),
    Array(-5,0,0,0,0,0,0,-5),
    Array(5,10,10,10,10,10,10,5),
    Array(0,0,0,0,0,0,0,0))

  private val whiteQueen = Array(
    Array(-20,-10,-10,-5,-5,-10,-10,-20),
    Array(-10,0,5,0,0,0,0,-10),
    Array(-10,5,5,5,5,5,0,-10),
    Array(0,0,5,5,5,5,0,-5),
    Array(-5,0,5,5,5,5,0,-5),
    Array(-10,0,5,5,5,5,0,-10),
    Array(-10,0,0,0,0,0,0,-10),
    Array(-20,-10,-10,-5,-5,-10,-10,-20))


  def printArray(array: Array[Array[Int]]) : Unit = {
    for(i <- 7 to 0 by -1){
      println()
      for(j <- array.indices){
        print(array(i)(j) + " , ")
      }
    }
  }

  def mirror(array: Array[Array[Int]]) : Array[Array[Int]] = {
    val mirror = array.clone()
    for(i <- array.indices){
      mirror(i) = mirror(i).reverse
    }
    mirror.reverse
  }

  def WhiteKnight: Array[Array[Int]] =  whiteKnight
  def BlackKnight: Array[Array[Int]] =  mirror(whiteKnight)

  def WhitePawn: Array[Array[Int]] =  whitePawn
  def BlackPawn: Array[Array[Int]] =  mirror(WhitePawn)

  def WhiteBishop: Array[Array[Int]] =  whiteBishop
  def BlackBishop: Array[Array[Int]] =  mirror(whiteBishop)

  def WhiteRook: Array[Array[Int]] =  whiteRook
  def BlackRook: Array[Array[Int]] =  mirror(whiteRook)

  def WhiteQueen: Array[Array[Int]] =  whiteQueen
  def BlackQueen: Array[Array[Int]] =  mirror(whiteQueen)

  def KingValue: Integer =  20000
  def QueenValue: Integer = 900
  def RookValue: Integer = 500
  def BishopValue: Integer = 330
  def KnightValue: Integer = 320
  def PawnValue: Integer = 100
  def MobilityValue: Integer = 10

}
