package uk.ac.ucl.cs.mr.acltutorial

import cc.factorie.la.{DenseTensor2, DenseTensor1}
import ml.wolfe.{Mat, Vect}
import ml.wolfe.term.{IntTerm, domain}
import uk.ac.ucl.cs.mr.acltutorial.MatrixRenderer.{Cell, Matrix}

/**
 * @author riedelcastro
 */
object ManualMF {


  def initialAV(K: Int, n:Int, m:Int) = {
    val rand = new scala.util.Random(0)
    val A = for (i <- 0 until n) yield new DenseTensor1(K)
    val V = for (j <- 0 until m) yield new DenseTensor1(K)
    for (i <- 0 until n; k <- 0 until K) {
      A(i)(k) = rand.nextGaussian()
    }

    for (j <- 0 until m; k <- 0 until K) {
      V(j)(k) = rand.nextGaussian()
    }
    (A, V)
  }

  def optimizeL2(M: Mat, K: Int, iterations: Int): (Seq[Vect], Seq[Vect]) = {
    val rand = new scala.util.Random(0)

    val n = M.dim1
    val m = M.dim2

    val alpha = 0.1
    val AV = initialAV(K, n, m)
    val A = AV._1
    val V = AV._2

    def update(i: Int, j: Int): Unit = {
      val a = A(i).copy
      val v = V(j).copy
      val y = a dot v
      A(i) += v * alpha * (M(i, j) - y)
      V(j) += a * alpha * (M(i, j) - y)
    }

    for (_ <- (0 until iterations).toList) {
      val i = rand.nextInt(n)
      val j = rand.nextInt(m)
      update(i, j)
    }
    (A, V)
  }


  import MatrixRenderer._

  def dotProductMatrix(A: Seq[Vect], V: Seq[Vect]) = {
    matrix(dots(A,V))
  }

  def dots(A: Seq[Vect], V: Seq[Vect]) = {
    val M = new DenseTensor2(A.length, V.length)
    for (i <- 0 until M.dim1; j <- 0 until M.dim2) M(i,j) = A(i) dot V(j)
    M
  }

  def matrix(M:Mat) = {
    Matrix(for (i <- 0 until M.dim1; j <- 0 until M.dim2) yield Cell(i,j,M(i,j)))
  }

  def format(m:Matrix) = {
    m.copy(m.cells collect {case Cell(r,c,d:Double) => Cell(r,c, f"$d%2.2f")})
  }

  def embeddings(A: Seq[Vect], V: Seq[Vect]): Matrix = {
    val rows = A.length
    val cols = V.length
    val plusRowEmbeddings = A.indices.map(i => rowEmbedding(i, cols, A(i).toSeq)).foldLeft(Matrix())(_ + _)
    val plusColEmbeddings = V.indices.map(j => colEmbedding(j, rows, V(j).toSeq)).foldLeft(plusRowEmbeddings)(_ + _)
    plusColEmbeddings
  }

  def main(args: Array[String]) {
    val n = 5
    val m = 5
    val M = new DenseTensor2(n, m)
    val rand = new scala.util.Random(0)

    //this should be the data
    for (i <- 0 until m; j <- 0 until m) {
      M(i, j) = rand.nextGaussian()
    }
    val (_A, _V) = optimizeL2(M, 2, 10)


  }

}
