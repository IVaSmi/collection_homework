package collections

/**
 * В данном задании Вам предлагается реализовать функции pure, map, flatMap и withFilter для кастомной обёртки
 */

trait Wrap[+A] {

  def get: A

  def pure[R](x: R): Wrap[R] = ???

  def flatMap[R](f: A => Wrap[R]): Wrap[R] = {
    ???
  }

  // HINT: map можно реализовать через pure и flatMap
  def map[R](f: A => R): Wrap[R] = {
    ???
  }

  def withFilter(f: A => Boolean): Wrap[A] = {
    ???
  }

}

object Wrap {
  def empty[R]: Wrap[R] = EmptyWrap
}

case class NonEmptyWrap[A](result: A) extends Wrap[A] {
  override def get: A = result
} // pure

object EmptyWrap extends Wrap[Nothing] {
  override def get: Nothing = throw new NoSuchElementException("Wrap.get")
} // bottom, null element


object for_comprehension {

  /**
   * Реализуйте методы map / flatMap / withFilter чтобы работал код и законы монад соблюдались
   */

  def main(args: Array[String]): Unit = {
    val result: Wrap[Int] = NonEmptyWrap(10)

    val r1 = for {
      res <- result
    } yield res

    val anotherResult: Wrap[Int] = NonEmptyWrap(100)

    val r2 = for {
      res <- result
      another <- anotherResult
    } yield res + another


    val r3 = for {
      res <- result
      another <- anotherResult
      if res > 10
    } yield res + another


    println(r1)
    println(r2)
    println(r3)


    //Monad law


    def decrementF(x: Int): Wrap[Int] = NonEmptyWrap(x - 1)

    def doubleIncrementF(x: Int): Wrap[Int] = NonEmptyWrap(x + 2)

    /**
     * Left unit law:
     * (unit(x) flatMap f) == f(x)
     * */
    def leftUnitLaw(): Unit = {
      assert(NonEmptyWrap(1).flatMap(decrementF) == decrementF(1))
      println("leftUnitLaw check success")
    }

    leftUnitLaw()

    /**
     * Right unit law:
     * (monad flatMap unit) == monad
     * */
    def rightUnitLaw(): Unit = {
      val monad: Wrap[Int] = NonEmptyWrap(10)
      assert(monad.flatMap(NonEmptyWrap(_)) == monad)
      println("rightUnitLaw check success")
    }

    rightUnitLaw()

    /**
     * Associativity law:
     * ((monad flatMap f) flatMap g) == (monad flatMap (x => f(x) flatMap g))
     * */
    def associativityLaw(): Unit = {
      val monad: Wrap[Int] = NonEmptyWrap(2)
      val left = monad.flatMap(decrementF).flatMap(doubleIncrementF)
      val right = monad.flatMap(x => decrementF(x).flatMap(doubleIncrementF))
      assert(left == right)
      println("associativityLaw check success")
    }


      leftUnitLaw()
      rightUnitLaw()
      associativityLaw()

  }

}