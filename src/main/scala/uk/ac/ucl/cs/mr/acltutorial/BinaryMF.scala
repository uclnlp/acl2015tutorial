package uk.ac.ucl.cs.mr.acltutorial

import uk.ac.ucl.cs.mr.acltutorial.MatrixRenderer._

import scala.util.Random

/**
 * @author riedelcastro
 */
object BinaryMF {

  val myRandom = new Random(0)

  def formatNumber(d: Double) = f"$d%2.2f"

  val persons = Seq("Anna", "Bob", "James", "Andreas")
  val organizations = Seq("UCL", "Oxford", "Sheffield", "MSR")

  val relations = Seq("worksFor", "lecturerAt", "professorAt", "studiedAt")

  val numPairs = 5

  val pairs = myRandom.shuffle(for (p <- persons; o <- organizations) yield (p, o)).take(numPairs)

  val randomGraph =
    for (row <- 0 until numPairs) yield for (col <- relations.indices) yield if (myRandom.nextDouble() < 0.3) 1.0 else 0.0

  def toCells(data: Seq[Seq[Double]], filter: Double => Boolean = _ => true) =
    for (row <- data.indices; col <- data(row).indices; value = data(row)(col))
      yield Cell(row, col, if (filter(value)) value else "")

  def toRowLabels(rows: Seq[String]) = for (r <- rows.indices) yield RowLabel(r, rows(r))

  def toColLabels(cols: Seq[String]) = for (r <- cols.indices) yield ColLabel(r, cols(r))

  val allCells = Matrix(toCells(randomGraph), toRowLabels(pairs.map(_.toString())), toColLabels(relations))

  val noZeroes = allCells.copy(cells = toCells(randomGraph, _ > 0.5))

  val someSampledZeroes = allCells.copy(cells = toCells(randomGraph, v => v > 0.5 || myRandom.nextDouble() < 0.5))

  val moreSampledZeroes = allCells.copy(cells = toCells(randomGraph, v => v > 0.5 || myRandom.nextDouble() < 0.8))

  val uSchemaRels = Seq("X, professor at Y", "X teaches at Y", "professorAt", "workedAt")
  val uSchemaNoZeroes = allCells.copy(cells =
    toCells(randomGraph, _ > 0.5).map(c => if (c.col > 1) c.copy(value = "?") else c), colLabels = toColLabels(uSchemaRels))


  val colEmbeddings = for (r <- relations.indices) yield Range(0, 2).map(_ => myRandom.nextGaussian())
  val rowEmbeddings = for (r <- 0 until numPairs) yield Range(0, 2).map(_ => myRandom.nextGaussian())

  def samplePositiveCell(data: Seq[Cell]): Cell = {
    val positive = data.collect { case c@Cell(_, _, 1.0) => c }
    positive(myRandom.nextInt(positive.length))
  }

  def sgdWithNegativeSampling(matrix:Matrix) = {
    def iteration() = {
      val pos1 = samplePositiveCell(matrix.cells)
      val m1 = matrix.copy(cells = Seq(pos1)) +
        rowEmbedding(pos1.row, relations.length, rowEmbeddings(pos1.row).map(formatNumber)) +
        colEmbedding(pos1.col, numPairs, colEmbeddings(pos1.col).map(formatNumber))
      val negInCol = matrix.cells.collect { case c@Cell(_, pos1.col, 0.0) => c }
      val neg1 = negInCol(myRandom.nextInt(negInCol.size))
      val m2 = matrix.copy(cells = Seq(neg1)) +
        rowEmbedding(neg1.row, relations.length, rowEmbeddings(neg1.row).map(formatNumber)) +
        colEmbedding(neg1.col, numPairs, colEmbeddings(neg1.col).map(formatNumber))
      Seq(m1, m2)
    }
    Seq(allCells) ++ iteration() ++ iteration()
  }

  val sgdUpdates = sgdWithNegativeSampling(allCells)

  def main(args: Array[String]) {

  }

}
