package Helpers

class ZEntry(move: ((Integer,Integer),(Integer,Integer)), value: Double, valueType: String,depth: Integer) {

  def getMove: ((Integer, Integer), (Integer, Integer)) = this.move
  def getValue: Double = this.value
  def getType: String = this.valueType
  def getDepth: Integer = this.depth

}
