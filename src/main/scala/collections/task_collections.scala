package collections

object task_collections {

  def isASCIIString(str: String): Boolean = str.matches("[A-Za-z]+")

  /**
   * Реализуйте метод который первый элемент списка не изменяет, а для последующих алгоритм следующий:
   * если isASCIIString is TRUE тогда пусть каждый элемент строки будет в ВЕРХНЕМ регистре
   * если isASCIIString is FALSE тогда пусть каждый элемент строки будет в нижнем регистре
   * Пример:
   * capitalizeIgnoringASCII(List("Lorem", "ipsum" ,"dolor", "sit", "amet")) -> List("Lorem", "IPSUM", "DOLOR", "SIT", "AMET")
   * capitalizeIgnoringASCII(List("Оказывается", "," "ЗвУк", "КЛАВИШЬ", "печатной", "Машинки", "не", "СТАЛ", "ограничивающим", "фактором")) ->
   * List("Оказывается", "," "звук", "КЛАВИШЬ", "печатной", "машинки", "не", "стал", "ограничивающим", "фактором")
   * HINT: Тут удобно использовать collect и zipWithIndex
   *
   * **/
  def capitalizeIgnoringASCII(text: List[String]): List[String] = {
    text.zipWithIndex.collect{
      case (el, index) if index == 0 => el
      case (el, index) if isASCIIString(el) => el.toUpperCase()
      case (el, index) if !isASCIIString(el) => el.toLowerCase()
    }
  }

  /**
   *
   * Компьютер сгенерировал текст используя вместо прописных чисел, числа в виде цифр, помогите компьютеру заменить цифры на числа
   *
   * Реализуйте метод который цифровые значения в строке заменяет на числа: 1 -> one, 2 -> two
   *
   * HINT: Для всех возможных комбинаций чисел стоит использовать Map
   * **/
  def numbersToNumericString(text: String): String = {
    val map = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "fifth", 6 -> "six")
    text.map(el => map.get(el).fold(el.toString)(r => r.mkString)).mkString
  }

  /**
   *
   * У нас есть два дилера со списками машин которые они обслуживают и продают (case class Auto(mark: String, model: String)).
   * Базы данных дилерров содержат тысячи и больше записей. Нет гарантии что записи уникальные и не имеют повторений
   *
   * Хотим узнать какие машины можно обслужить учитывая этих двух дилеров
   *
   * Реализуйте метод который пример две коллекции в себя(два истоника) и вернёт объеденённый уникальный список
   *
   * HINT: Set
   * HINT2: Iterable стоит изменить
   * **/

  case class Auto(mark: String, model: String)

  /**
   * Хотим узнать какие машины можно обслужить учитывая этих двух дилеров
   * Реализуйте метод который примет две коллекции (два источника) и вернёт объеденённый уникальный список
   **/
  def intersectionAuto(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
   dealerOne.toSet.union(dealerTwo.toSet)
  }

  /**
   * Хотим узнать какие машины обслуживается в первом дилеромском центре, но не обслуживаються во втором
   * Реализуйте метод который примет две коллекции (два источника)
   * и вернёт уникальный список машин обслуживающихся в первом дилерском центре и не обслуживабщемся во втором
   **/
  def filterAllLeftDealerAutoWithoutRight(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
    dealerOne.toSet.diff(dealerTwo.toSet)
  }
}
