package Pieces

import Game.Helpers.Color.Color
import Game.Helpers.Type.Type
import Game._


abstract class Piece(c: Color) {

  var moved = false

  def color: Color = c

  def pieceType: Type

  def blockedByPiece(state: Array[Array[Spot]], move: Move): Boolean

  def isValidMoveSet(state: Array[Array[Spot]], move: Move): Boolean = {
    if (blockedByPiece(state, move)) false
    else true
  }

  def isBlockedDiagonal(state: Array[Array[Spot]], move: Move): Boolean ={
    val from = move.from
    val to = move.to
    var x_dir, y_dir = 1

    if(to._1 - from._1 < 0) {
      x_dir = -1
    }

    if(to._2 - from._2 < 0) {
      y_dir = -1
    }

    var newX = from._1 + x_dir
    var newY = from._2 + y_dir

    while(newX != to._1 && newY != to._2){
      if(newX >= 0 && newY >= 0 && newX <= 7 && newY <= 7) {
        if (state(newX)(newY).isOccupied) {
          return true
        }
      }
      newX += x_dir
      newY += y_dir
    }

    false
  }


  def isBlockedHorisontalOrVertical(state: Array[Array[Spot]], move: Move): Boolean ={
    val from = move.from
    val to = move.to

    if (to._2 != from._2){
      if(to._2 > from._2){
        for(i <- from._2 + 1 until to._2 by 1){
          if(state(from._1)(i).isOccupied){
            return true
          }
        }
      }
      else{
        for(i <- from._2 - 1 until to._2 by -1){
          if(state(from._1)(i).isOccupied){
            return true
          }
        }
      }
    }

    else{
      if(to._1 > from._1){
        for(i <- from._1 + 1 until to._1 by 1){
          if(state(i)(from._2).isOccupied){
            return true
          }
        }
      }
      else{
        for(i <- from._1 - 1 until to._1 by -1){
          if(state(i)(from._2).isOccupied){
            return true
          }
        }

      }
    }
    false
  }

}
