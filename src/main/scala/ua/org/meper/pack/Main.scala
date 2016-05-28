package ua.org.meper.pack


/**
  * Created by annapedchenko on 4/10/16.
  */
object Main {

  def main(args: Array[String]) {
    case class Rectangle(xSize: Int, ySize: Int) {}
    case class Position(rectangle: Rectangle, x: Int, y: Int) {}
    case class Individual(genotype: Seq[Int]) {}


    val allRectangles = Seq(Rectangle(2, 3), Rectangle(4, 6), Rectangle(5, 2), Rectangle(4, 1), Rectangle(1, 4),
      Rectangle(2, 6), Rectangle(3, 6), Rectangle(5, 1), Rectangle(3, 1), Rectangle(6, 4))

    def overlap(first: Position, second: Position): Boolean = !notOverlap(first, second)

    def notOverlap(first: Position, second: Position): Boolean = {
      val rightTop = (second.x >= first.x + first.rectangle.xSize) || (second.y >= first.y + first.rectangle.ySize)
      val leftBottom = (second.x <= first.x - second.rectangle.xSize) || (second.y <= first.y - second.rectangle.ySize)
      (rightTop || leftBottom) && (second.x >= 0) && (second.y >= 0)
    }

    def packThisWithOthers(others: Seq[Position], second: Rectangle, secondX: Int = 10, secondY: Int = 10): Seq[Position] = {
      if (others.isEmpty) {
        Seq(Position(second, 0, 0))
      } else {
        val left = others.exists(overlap(_, Position(second, secondX - 1, secondY)))
        val bottom = others.exists(overlap(_, Position(second, secondX, secondY - 1)))
        if (left && bottom) {
          println(others)
          others :+ Position(second, secondX, secondY)
        } else if (!bottom) {
          println(second, secondX, secondY)
          packThisWithOthers(others, second, secondX, secondY - 1)
        } else {
          packThisWithOthers(others, second, secondX - 1, secondY)
        }
      }
    }


    def right: (Seq[Int] => Seq[Position]) =
      z => z.foldLeft(Seq.empty[Position])((packed, index) => packThisWithOthers(packed, allRectangles(index)))
    def outOfRange(positions: Seq[Position], rectangle: Rectangle): Boolean = {
      positions.exists(p => {
        p.x + p.rectangle.xSize > rectangle.xSize || p.y + p.rectangle.ySize > rectangle.ySize
      })
    }
    def generateIndividual(range: Rectangle, permutation: Seq[Int], allRectangles: Seq[Rectangle]): Individual = {
      def step(i: Int, packed: Seq[Position],curPermutation:Seq[Int]): (Seq[Position],Seq[Int]) = {
        if (i == permutation.size) {
          (packed,curPermutation)
        }
        else {
          val others: Seq[Position] = packThisWithOthers(packed, allRectangles(permutation(i)), range.xSize-allRectangles(permutation(i)).xSize, range.ySize)
          if (outOfRange(others, range))
            (packed,curPermutation)
          else{
            step(i + 1, others,curPermutation :+ permutation(i))
          }
        }
      }
      Individual(step(0,Seq.empty,Seq.empty)._2)
    }

    val permutation: Seq[Int] = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    println(right(permutation))
    println(generateIndividual(Rectangle(10,10),Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),allRectangles))

    //    println(generateIndSeq(3))
    //    println(generateIndSeq(4))

  }


}
