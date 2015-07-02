package uk.ac.ucl.cs.mr.acltutorial

import uk.ac.ucl.cs.mr.acltutorial.MatrixRenderer.{Matrix, ColLabel, RowLabel, Cell}

import scala.util.Random

/**
 * @author riedelcastro
 */
object BinaryMF {

  val random = new Random(0)

  val persons = Seq("Anna", "Bob", "James", "Andreas")
  val organizations = Seq("UCL", "Oxford", "Sheffield", "MSR")

  val relations = Seq("worksFor", "lecturerAt", "professorAt", "studiedAt")

  val numPairs = 5

  val pairs = random.shuffle(for (p <- persons; o <- organizations) yield (p, o)).take(numPairs)

  val randomGraph =
    for (row <- 0 until numPairs) yield for (col <- relations.indices) yield if (random.nextDouble() < 0.3) 1.0 else 0.0

  def toCells(data:Seq[Seq[Double]], filter:Double => Boolean = _ => true) =
    for (row <- data.indices; col <- data(row).indices; value = data(row)(col))
      yield Cell(row,col,  if (filter(value)) value else "")

  def toRowLabels(rows:Seq[String]) = for (r <- rows.indices) yield RowLabel(r, rows(r))
  def toColLabels(cols:Seq[String]) = for (r <- cols.indices) yield ColLabel(r, cols(r))

  val allCells = Matrix(toCells(randomGraph),toRowLabels(pairs.map(_.toString())),toColLabels(relations))

  val noZeroes = allCells.copy(cells = toCells(randomGraph, _ > 0.5))

  val someSampledZeroes = allCells.copy(cells = toCells(randomGraph, v => v > 0.5 || random.nextDouble() < 0.5))

  val moreSampledZeroes = allCells.copy(cells = toCells(randomGraph, v => v > 0.5 || random.nextDouble() < 0.8))


  def main(args: Array[String]) {

  }

}
