type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {

  def transpose: Matrix = m match {
    case Some(mat) =>
      if (mat.isEmpty) new Matrix(Some(List.empty))
      else {
        val helper: List[List[Double]] = List.fill(mat.head.length)(List.empty[Double])
        val matTransposed = mat.foldLeft(helper) { (acc, row) =>
          row.zip(acc).foldLeft(List.empty[List[Double]]) { (acc2, tuple) =>
            tuple match {
              case (elem, col) => acc2 :+ (col :+ elem)
            }
          }
        }
        new Matrix(Some(matTransposed))
      }
    case None => new Matrix(None)

  }

  def map(f: Double => Double): Matrix = m match {
    case Some(data) =>
      // parcurg liniile matricii
        val mapped1 = data.foldLeft(List[List[Double]]()) { (acc, row) =>
          // parcurg elementele dintr-o linie
          val mapped2 = row.foldLeft(List[Double]()) { (acc2, elem) =>
            acc2 :+ f(elem)
          }
          acc :+ mapped2
        }
        Matrix(Some(mapped1))
    case None => Matrix(None)
  }

  def -(other: Matrix): Matrix = (m, other.data) match {
    case (Some(mat1), Some(mat2)) =>
      if ((mat1.length != mat2.length) || (mat1.head.length != mat2.head.length)) {
        new Matrix(None)
      } else {
        val res = mat1.zip(mat2).map { case (row1, row2) =>
          row1.zip(row2).foldLeft(List[Double]()) { case (acc, (elem1, elem2)) =>
            acc :+ (elem1 - elem2) }
        }
        new Matrix(Some(res))
      }
    case _ => new Matrix(None)
  }

  def *(other: Matrix): Matrix = (m, other.data) match {
    case (Some(mat1), Some(mat2)) =>
      if (mat1.isEmpty || mat2.isEmpty || mat1.head.length != mat2.length || mat1.head.isEmpty || mat2.head.isEmpty) {
        new Matrix(None)
      } else {
        val result = mat1.map { row1 =>
          mat2.transpose.map { col2 =>
            row1.zip(col2).map { case (elem1, elem2) =>
              elem1 * elem2
            }.foldRight(0.0)(_ + _)
          }
        }
        new Matrix(Some(result))
      }
    case _ => new Matrix(None)
}

  def ++(x: Double): Matrix = m match {
    case Some(mat) =>
      val res = mat.map(row => row :+ x)
      new Matrix(Some(res))
    case None => new Matrix(None)
  }

  def data: Option[Mat] = m
  def height: Option[Int] = data.map(_.length)
  def width: Option[Int] = data.map(_.head.length)
  override def toString: String = m match {
    case Some(matrix) =>
     val stringBuilder = new StringBuilder()
      matrix.foreach(row => { stringBuilder.append(row.mkString(" ")).append("\n")})
      stringBuilder.toString()
    case None => "Error"
  }
}

object Matrix {
  def apply(data: Mat): Matrix = new Matrix(Some(data))
  def apply(data: Option[Mat]): Matrix = new Matrix(data)

  def isValidDouble(string: String): Boolean = {
    try {
      string.toDouble
      true
    } catch {
      case _: NumberFormatException => false
    }
  }
  
  def apply(dataset: Dataset): Matrix = {
    val dataWithoutHeader = dataset.data.drop(1)
    val data: List[List[Double]] = dataWithoutHeader.map { row =>
      row.flatMap { str =>
        if (isValidDouble(str)) Some(str.toDouble)
        else None
      }
    }
    new Matrix(Some(data))
  }
}
