package dojo

object Anagram {
  def calculate(words : Seq[String]) : Set[Set[String]] = {
    words.groupBy(_.sorted)
         .collect {
            case (_, wordSet) if wordSet.size > 1 => wordSet.toSet
          }.toSet
  }
} 