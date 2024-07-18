import scala.io.Source
import scala.math.ceil

class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m

  override def toString: String = {
    val stringBuilder = new StringBuilder()
    data.foreach { element =>
      stringBuilder.append(element.mkString(",")).append("\n")
    }
    stringBuilder.toString()
  }

  def getIndex(list: List[String], element: String): Int = {
    val index = list.indexOf(element)
    index
  }

  def selectColumn(col: String): Dataset = {
    val columnIndex = getIndex(data.head, col)
    val selectedColumn = data.map(_(columnIndex)) // selectez coloana folosindu-ma de index
    Dataset(selectedColumn.map(List(_)))
  }

  def selectColumns(cols: List[String]): Dataset = {
    val columnIndices = cols.map(col => getIndex(data.head, col))
    // parcurg liniile si selectez atributele coloanelor dorite
    val selectedColumns = data.map(row => columnIndices.map(row))
    Dataset(selectedColumns)
  }

  def split(percentage: Double): (Dataset, Dataset) = {
    val sortedData = data.drop(1).sortBy(_.head) // elimin header-ul si sortez dupa prima coloana
    val header = data.head
    val wantedIndex = ceil(1 / percentage).toInt // calculez indexul dupa care vreau sa fac split-ul

    val (evaluationData, trainingData, index) = sortedData.foldLeft(List.empty[List[String]], List.empty[List[String]], 1) {
      case ((evalAcc, trainAcc, index), item) =>
        if (index % wantedIndex == 0) {
          (evalAcc :+ item, trainAcc, index + 1)
        } else {
          (evalAcc, trainAcc :+ item, index + 1)
        }
    }

    (new Dataset(header :: trainingData), new Dataset(header :: evaluationData))
}

  def size: Int = data.length
  def getRows: List[List[String]] = data
  def getHeader: List[String] = data.head
}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    val data = Source.fromFile(csv_filename)
                     .getLines()
                     .map(_.split(",").toList)
                     .toList
    new Dataset(data)
  }

  def apply(ds: List[List[String]]): Dataset = {
    new Dataset(ds)
  }
}
