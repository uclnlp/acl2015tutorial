package uk.ac.ucl.cs.mr.acltutorial

import breeze.linalg.{DenseMatrix, svd}
import cc.factorie.la.{DenseTensor1, DenseTensor2}
import ml.wolfe.{Mat, Vect}
import uk.ac.ucl.cs.mr.acltutorial.MatrixRenderer.{ColLabel, Matrix, RowLabel}

/**
 * @author riedelcastro
 */
object ManualCMF {

  def initial_embeddings(N: Int, rank: Int, scale: Double = 0.1) = {
    val rand = new scala.util.Random(0)
    val U = for (i <- 0 until N) yield new DenseTensor1(rank)
    for (i <- (0 until N).toList; k <- (0 until rank).toList) {
      U(i)(k) = scale * rand.nextGaussian()
    }
    U
  }

  def header(rows: Seq[String], cols: Seq[String]) = Matrix(
    rowLabels = rows.zipWithIndex map { case (r, i) => RowLabel(i, r) },
    colLabels = cols.zipWithIndex map { case (r, i) => ColLabel(i, r) }
  )


  import MatrixRenderer._

  def dotProductMatrix(A: Seq[Vect], V: Seq[Vect]) = {
    matrix(dots(A, V))
  }

  def dots(A: Seq[Vect], V: Seq[Vect]) = {
    val M = new DenseTensor2(A.length, V.length)
    for (i <- (0 until M.dim1).toList; j <- (0 until M.dim2).toList) M(i, j) = A(i) dot V(j)
    M
  }

  def delta(m1: Mat, m2: Mat) = {
    val result = new DenseTensor2(m1.dim1, m1.dim2)
    for (i <- 0 until m1.dim1; j <- 0 until m1.dim2)
      result(i, j) = m1(i, j) - m2(i, j)
    result
  }

  def matrix(M: Mat) = {
    Matrix(for (i <- (0 until M.dim1).toList; j <- (0 until M.dim2).toList) yield Cell(i, j, M(i, j)))
  }

  def colVector(v: Vect) = {
    Matrix(for (i <- 0 until v.dim1) yield Cell(i, 0, v(i)))
  }

  def rowVector(v: Vect) = {
    Matrix(for (i <- 0 until v.dim1) yield Cell(0, i, v(i)))
  }

  def numbers(m: Matrix) = {
    m.copy(m.cells collect { case cell@Cell(_, _, d: Double, _, _, _, _) => cell.copy(value = f"$d%2.2f") })
  }

  def opacity(m: Matrix, min: Double = -1, max: Double = 1, color: (Int, Int, Int) = (0, 0, 0)) = {
    def valueToOpacity(value: Double) = {
      val capped = Math.min(Math.max(min, value), max)
      (capped - min) / (max - min)
    }
    m.copy(m.cells collect { case cell@Cell(_, _, d: Double, _, _, _, _) =>
      cell.copy(value = "", color = color, opacity = valueToOpacity(d))
    })
  }

  def parseMatrix(string: String) = {
    val rows = string.split("\n").map(_.trim.split("\\s"))
    val n = rows.length
    val m = rows.head.length
    val mat = new DenseTensor2(n, m)
    for (i <- 0 until n; j <- 0 until m) mat(i, j) = rows(i)(j).toDouble
    mat
  }

  def parseRenderMatrix(string: String) = {
    val rows = string.split("\n").map(_.trim.split("\\s"))
    val n = rows.length
    val m = rows.head.length
    val cells = for (i <- 0 until n; j <- 0 until m; if rows(i)(j) != "_") yield Cell(i, j, rows(i)(j))
    Matrix(cells)
  }

  def sigm(mat: Mat) = {
    val result = new DenseTensor2(mat.dim1, mat.dim2)
    for (i <- 0 until mat.dim1; j <- 0 until mat.dim2)
      result(i, j) = ml.wolfe.util.Math.sigmoid(mat(i, j))
    result
  }

  def l2Loss(guess: Mat, gold: Mat) = {
    guess.l2Similarity(gold)
  }

  def embeddings(A: Seq[Vect], V: Seq[Vect], withBoxes: Boolean = false): Matrix = {
    val rows = A.length
    val cols = V.length
    val plusRowEmbeddings = A.indices.map(i => rowEmbedding(i, cols, A(i).toSeq, withBoxes)).foldLeft(Matrix())(_ + _)
    val plusColEmbeddings = V.indices.map(j => colEmbedding(j, rows, V(j).toSeq, withBoxes)).foldLeft(plusRowEmbeddings)(_ + _)
    plusColEmbeddings.copy(hRulers = Seq(rows), vRulers = Seq(cols))
  }

  def breezeSVD(mat: Mat): (Mat, Vect, Mat) = {
    val asBreeze = toBreezeMat(mat)
    val (u, s, v) = svd(asBreeze)
    (toFactorieMat(u), new DenseTensor1(s.toArray), toFactorieMat(v))
  }

  def toEmbeddings(u: Mat, s: Vect, v: Mat, k: Int): (Seq[Vect], Seq[Vect]) = {
    val _a = for (i <- 0 until u.dim1) yield new DenseTensor1(k)
    val _v = for (i <- 0 until v.dim1) yield new DenseTensor1(k)
    for (i <- _a.indices; l <- 0 until k) _a(i)(l) = u(i, l) * math.sqrt(s(l))
    for (j <- _v.indices; l <- 0 until k) _v(j)(l) = v(l, j) * math.sqrt(s(l))
    (_a, _v)
  }

  def toBreezeMat(mat: Mat) = {
    DenseMatrix.create(mat.dim1, mat.dim2, mat.toArray).t
  }

  def toFactorieMat(mat: DenseMatrix[Double]) = {
    val result = new DenseTensor2(mat.rows, mat.cols)
    for (i <- 0 until mat.rows; j <- 0 until mat.cols) result(i, j) = mat(i, j)
    result
  }

  def main(args: Array[String]) {
    //    val n = 5
    //    val m = 5
    //    val M = new DenseTensor2(n, m)
    //    val rand = new scala.util.Random(0)
    //
    //    //this should be the data
    //    for (i <- 0 until m; j <- 0 until m) {
    //      M(i, j) = rand.nextGaussian()
    //    }
    //    val (_A, _V) = optimizeL2(M, 2, 10)

    val M = parseMatrix(
      """1 2 0 0 0
       0 0 1 0 0
       0 0 0 1 1
       0 0 1 1 1
       1 2 1 1 1""")
    val m = toBreezeMat(M)

    val (u, s, v) = breezeSVD(M)
    println(s)

    val (_a, _v) = toEmbeddings(u, s, v, 4)
    println(dots(_a, _v))



    //
    //    println(m)
    //    println(toFactorieMat(m))
    //
    //    val bM = DenseMatrix((1.0,2.0), (2.0,4.0))
    //    val (u,s,v) = svd(bM)
    //    println(s)

  }

}
