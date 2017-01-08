package Game

import Pieces.Piece

class Spot(x: Integer, y: Integer) {

  def getX: Integer = this.x

  def getY: Integer = this.y

  var piece: Piece = _

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
