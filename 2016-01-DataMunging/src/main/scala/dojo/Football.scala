package dojo



object Football {

  def readData() = ParserF.findMin(filename, extractor)(_.spread)

  val RegEx = ("[^.]+\\. ([A-Za-z_]*)"+
    "[^0-9]+([0-9]+)" +
    "[^0-9]+([0-9]+)" +
    "[^0-9]+([0-9]+)" +
    "[^0-9]+([0-9]+)" +
    "[^0-9]+([0-9]+)" +
    "[^0-9]+([0-9]+)" +
    ".*").r

  case class Data(name: String, forGoals: Int, againstGoals: Int) {
    def spread = Math.abs(forGoals - againstGoals)
  }

  val extractor: PartialFunction[String, Data] = {
    case RegEx(name, _, _, _, _, forString, againstStr) => Data(name, forString.toInt, againstStr.toInt)
  }

  def filename: String = "src/resource/data/football.dat"
}