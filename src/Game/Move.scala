package Game

class Move(f: (Int,Int), t: (Int,Int)) {

  def from: (Int,Int) = this.f

  def to: (Int,Int) = this.t

}
