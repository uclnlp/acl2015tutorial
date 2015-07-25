package uk.ac.ucl.cs.mr.acltutorial

import cc.factorie.la.{DenseTensor1, DenseTensor2}
import ml.wolfe.{Mat, Vect}
import uk.ac.ucl.cs.mr.acltutorial.MatrixRenderer.{Cell, ColLabel, Matrix, RowLabel}

/**
 * @author gbouchar
 */


object ManualCMF {

  val logistic_type = 0
  val l2_type = 0
  val rand = new scala.util.Random(0)




  // sample a normalized discrete distribution
  def sample_discrete(probas: Seq[Double]): Int = {
    var mass = rand.nextDouble() - probas.head
    var pos = 0
    while (mass > 0 && pos < probas.length - 1){
      pos += 1
      mass -= probas(pos)
    }
    pos
  }

  // computation of the model

  def initial_embeddings(N: Int, rank: Int, scale: Double = 0.1): Seq[DenseTensor1] = {
    val rand = new scala.util.Random(0)
    val U = for (i <- 0 until N) yield new DenseTensor1(rank)
    for (i <- (0 until N).toList; k <- (0 until rank).toList) {
      U(i)(k) = scale * rand.nextGaussian()
    }
    U
  }



  def loss_gradient(loss_type: Int, score:Double, gold:Double): Double = {
    var grad = 0.0
    if (loss_type == logistic_type) {
      grad = gold - 1.0 / (1.0 + math.exp(-score))
    }else if(loss_type == l2_type) {
      grad = gold - score
    }
    grad
  }

  // Sampling in a sequence
  type T = (Int, Int, Double) //todo: Make a generic type
  def sample_seq(S: Seq[T]): T = {
    S(rand.nextInt(S.length))
  }

  case class Table(
                    loss_type:    Int,
                    observations: Seq[(Int,Int,Double)])

  def num_of_entities(list_of_tables: Seq[Table]): Int = {
    var N = 0
    for (table <- list_of_tables){
      for ((i, j, v) <- table.observations){
        if (i > N) N = i
        if (j > N) N = j
      }
    }
    N + 1
  }

  def optimizeCMF0(database: Seq[Table], tables_weights: Seq[Double], rk: Int, iters: Int, step_size:Double = 0.1, initScale:Double = 1.0): Seq[Vect] = {
    val U = initial_embeddings(num_of_entities(database), rk, initScale) // N*rank embedding matrix
    for (it <- 0 until iters) {    // Loop over SGD iterations
      // Sampling an observation
      val relation = sample_discrete(tables_weights)
      val table = database(relation)
      val observation = sample_seq(table.observations)
      //Gradient Step
      val (i, j, gold) = observation
      val v1 = U(i).copy
      val v2 = U(j).copy
      val dloss = loss_gradient(table.loss_type, v1 dot v2, gold)
      U(i) += v2 * step_size * dloss
      U(j) += v1 * step_size * dloss
    }
    U //output the embedding matrix}
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
    val entityNames = Seq(
       Seq("Greece","Tsipras","Germany","crisis","economy"),
       Seq("Topic G","Topic P","Topic E"),
       Seq("D1","D2","D3","D4","D5"))

    val n1 = entityNames(0).length //number of entities of type 1 (words)
    val n2 = entityNames(1).length  //number of entities of type 2 (labels)
    val n3 = entityNames(2).length  //number of entities of type 1 (documents)

    val tuples31 = toTuples(parseMatrix(
      """1 2 0 0 0
         0 0 1 0 0
         0 0 0 1 1
         0 0 1 1 1
         1 2 1 1 1"""), n1 + n2, 0)

    val tuples32 = toTuples(parseMatrix(
      """1 0 0
         0 1 1
         0 1 1
         0 1 0
         1 0 1"""), n1 + n2, n1)

    val tuples21 = toTuples(parseMatrix(
      """1 1 0 0 0
         1 1 0 1 1
         1 1 1 1 1"""), n1, 0)

    val T1 = new Table(loss_type = l2_type, observations = tuples31)
    val T2 = new Table(loss_type = logistic_type, observations = tuples32)
    val T3 = new Table(loss_type = logistic_type, observations = tuples21)

    val U = optimizeCMF0(Seq(T1, T2, T3), Seq(.1, .5, .4), 2, 10000)
    println(U)

    println(tuplesToMatrix(tuples21))
    println("finished")


  }
  // utilities
  def tuplesToMatrix(tuples:Seq[(Int,Int,Double)]): Matrix = {
    val max_i = tuples.map(_._1).reduceLeft(math.max) + 1
    val min_i = tuples.map(_._1).reduceLeft(math.min)
    val max_j = tuples.map(_._2).reduceLeft(math.max) + 1
    val min_j = tuples.map(_._2).reduceLeft(math.min)
    val cells = for ((i, j, v) <- tuples) yield Cell(i - min_i, j - min_j, v)
    Matrix(cells)
  }

  def transpose(tuples:Seq[(Int,Int,Double)]) = {
    for ((i,j,v) <- tuples) yield (j,i,v)
  }


    //rendering functions


  def header(rows: Seq[String], cols: Seq[String]) = Matrix(
    rowLabels = rows.zipWithIndex map { case (r, i) => RowLabel(i, r) },
    colLabels = cols.zipWithIndex map { case (r, i) => ColLabel(i, r) }
  )



  def dotProductMatrix(A: Seq[Vect], V: Seq[Vect]) = {
    matrix(dots(A, V))
  }

  def dots(A: Seq[Vect], V: Seq[Vect]) = {
    val M = new DenseTensor2(A.length, V.length)
    for (i <- (0 until M.dim1).toList; j <- (0 until M.dim2).toList) M(i, j) = A(i) dot V(j)
    M
  }



  def parseMatrix(string: String,offset_row: Int = 0, offset_col: Int = 0) = {
    val rows = string.split("\n").map(_.trim.split("\\s"))
    val n = rows.length + offset_row
    val m = rows.head.length + offset_col
    val mat = new DenseTensor2(n, m)
    for (i <- 0 until n; j <- 0 until m) mat(i + offset_row, j + offset_col) = rows(i)(j).toDouble
    mat
  }

  def toTuples(mat: DenseTensor2, offset_row: Int = 0, offset_col: Int = 0): Seq[(Int, Int, Double)] = {
    val S = Range(0,mat.dim1) flatMap(i => Range(0,mat.dim2).map(
      j => (i + offset_row, j + offset_col, mat(i,j))))
    S
  }

  def delta(m1: Mat, m2: Mat) = {
    val result = new DenseTensor2(m1.dim1, m1.dim2)
    for (i <- 0 until m1.dim1; j <- 0 until m1.dim2)
      result(i, j) = m1(i, j) - m2(i, j)
    result
  }





  // rendering functions
  import MatrixRenderer._


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


  def toEmbeddings(u: Mat, s: Vect, v: Mat, k: Int): (Seq[Vect], Seq[Vect]) = {
    val _a = for (i <- 0 until u.dim1) yield new DenseTensor1(k)
    val _v = for (i <- 0 until v.dim1) yield new DenseTensor1(k)
    for (i <- _a.indices; l <- 0 until k) _a(i)(l) = u(i, l) * math.sqrt(s(l))
    for (j <- _v.indices; l <- 0 until k) _v(j)(l) = v(l, j) * math.sqrt(s(l))
    (_a, _v)
  }


}
