package Pieces

import Game.Helpers.Color.Color
import Game.Helpers.Type.Type
import Game._
import Helpers.Type


class King(color: Color) extends Piece(color: Color) {

  def pieceType: Type = Type.King

  override val pieceValue: Integer = 20000

  override
  def isValidMoveSet(state: Array[Array[Spot]], from: (Integer,Integer), to: (Integer,Integer)): Boolean = {

    if (!super.isValidMoveSet(state,from, to)) { return false }

    var valid = false

    val legalPairs: Array[(Int, Int)] = Array( (0,1),(0,-1),(1,0),(-1,0), (1,1),(-1,-1),(1,-1),(-1,1))

    for (p <- legalPairs ) {
      if(to._1 == from._1 + p._1 && to._2 == from._2 + p._2){
        valid = true
      }
    }
    valid
  }

  def blockedByPiece(state: Array[Array[Spot]], from: (Integer,Integer), to: (Integer,Integer)): Boolean = {
    false
  }

  // TODO implement castling logic

}
