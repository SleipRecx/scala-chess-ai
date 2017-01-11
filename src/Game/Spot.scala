package Game

import Game.Helpers.{Color, Type}
import Pieces.{Piece, Queen}

class Spot(c: (Int,Int)) {

  var piece: Piece = _

  def cord: (Int,Int) = this.c

  def addPiece(p: Piece): Unit = {
    if(p.pieceType == Type.Pawn){
      if ((c._1 == 7 && p.color == Color.White) || (c._1 == 0 && p.color == Color.Black)){
        this.piece = new Queen(p.color)
        return
      }
    }
    this.piece = p
  }

  def removePiece(): Unit ={
    this.piece = null
  }

  def isOccupied: Boolean ={
    this.piece != null
  }
}
