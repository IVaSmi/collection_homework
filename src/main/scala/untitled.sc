import scala.util.control.NoStackTrace

object task_collections {

  val numericMapping = Map(
    1 -> "one",
    2 -> "two",
    3 -> "three",
    4 -> "four",
    5 -> "five",
    6 -> "six",
    7 -> "seven",
    8 -> "eight",
    9 -> "nine",
    0 -> "zero",
    10 -> "ten"
  )

  def numbersToNumericString(text: String): String =
    text
      .split(" ")
      .map(word =>
        if (word matches "[-+]?\\d+")
          numericMapping.getOrElse(
            word.toInt,
            new Exception(s"Text format for the number $word is undefined")
          )
        else word
      )
      .mkString(" ")
}
println(task_collections.numbersToNumericString("Hello. I am 10 years old"))
