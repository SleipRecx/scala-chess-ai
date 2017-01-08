package Pieces

import Game.Helpers.Color.Color
import Game.Helpers.Type.Type
import Game._
import Helpers.Type

class Queen(color: Color) extends Piece(color: Color) {

  override val pieceValue: Integer = 900

  def pieceType: Type = Type.Queen

  def blockedByPiece(state: Array[Array[Spot]], from: (Integer,Integer), to: (Integer,Integer)): Boolean = {

    val x_diff = Math.abs(to._1 - from._1)
    val y_diff = Math.abs(to._2 - from._2)

    if (x_diff == y_diff) {
      return isBlockedDiagonal(state,from,to)
    }

    isBlockedHorisontalOrVertical(state,from,to)
  }


  override
  def isValidMoveSet(state: Array[Array[Spot]], from: (Integer,Integer), to: (Integer,Integer)): Boolean = {

    if (!super.isValidMoveSet(state,from,to)) { return false }

    val x_diff = Math.abs(to._1 - from._1)
    val y_diff = Math.abs(to._2 - from._2)

    if(!(x_diff == y_diff) && !(to._1 == from._1) && !(to._2 == from._2)){
      return false
    }

    true
  }

}
