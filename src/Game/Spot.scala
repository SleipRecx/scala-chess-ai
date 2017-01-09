package Game

import Pieces.Piece

class Spot(c: (Int,Int)) {

  var piece: Piece = _

  def cord: (Int,Int) = this.c

  def addPiece(piece: Piece): Unit = {
    this.piece = piece
  }

  def removePiece(): Unit ={
    this.piece = null
  }

  def isOccupied: Boolean ={
    this.piece != null
  }
}
