object Regression {

  def sumMatrix(m: Matrix): Double = {
    def sumMat(mat: Mat, height: Int, width: Int): Double = {
      (0 until height).foldLeft(0.0) { (rowAcc, rowIndex) =>
        (0 until width).foldLeft(rowAcc) { (acc, colIndex) =>
          acc + mat(rowIndex)(colIndex)
        }
      }
    }

    val height = m.height.getOrElse(0)
    val width = m.width.getOrElse(0)

    m.data match {
      case Some(mat: Mat) => sumMat(mat, height, width)
      case None => 0.0
    }
  }

  def gradient_descendent(steps: Int, X: Matrix, Y: Matrix, W: Matrix, alpha: Double): (Matrix, Matrix) = {
    if (steps <= 0) (W, X.*(W))
    else {
      val estimations = X.*(W) // pretul prezis
      val error = estimations.-(Y) // eroarea
      val gradient = {
        val aux1 = X.transpose.*(error)
        val aux2 = aux1.map(x => x / X.height.get)
        aux2
      }
      val prod = gradient.map(x => x * alpha)
      val newW = W.-(prod)
      gradient_descendent(steps - 1, X, Y, newW, alpha)
    }
  }

  def regression(dataset_file: String,
                 attribute_columns: List[String],
                 value_column: String,
                 test_percentage: Double,
                 alpha: Double,
                 gradient_descent_steps: Int): (Matrix, Double) = {

    val dataset = Dataset(dataset_file)
    val (train, evaluate) = dataset.split(test_percentage)

    val trainX = train.selectColumns(attribute_columns)
    val evaluationX = evaluate.selectColumns(attribute_columns)
    val trainY = train.selectColumn(value_column) //pretul real
    val evaluationY = evaluate.selectColumn(value_column)

    // construirea matricilor din seturile de date
    val trainXMatrix = Matrix(trainX).++(1.0)
    val evaluationXMatrix = Matrix(evaluationX).++(1.0)
    val trainYMatrix = Matrix(trainY)
    val evaluationYMatrix = Matrix(evaluationY)

    // construirea vectorului W si setarea valorlor acestuia la zero
    val widthTrain = trainXMatrix.width.getOrElse(0)
    val helper = List.fill(widthTrain)(List(0.0))
    val trainW = Matrix(helper)

    val (newTrainW, trainError) = gradient_descendent(gradient_descent_steps, trainXMatrix, trainYMatrix, trainW, alpha)

    // calculez eroarea pentru setul de validare
    val estimation = evaluationXMatrix.*(newTrainW)
    val error = estimation.-(evaluationYMatrix)
    val sum: Double = sumMatrix(error)
    val elements: Double = error.width.getOrElse(0) * error.height.getOrElse(0)
    val totalError: Double = sum / elements

    (newTrainW, totalError)
  }

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))

  }

}