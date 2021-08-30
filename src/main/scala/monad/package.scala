package object monad {

  /**
    * Реализуйте методы map / flatMap / withFilter чтобы работал код и законы монад соблюдались
    * HINT: для проверки на пустой элемент можно использовать eq
    */

  trait Wrap[+A] { self =>

    def get: A

    def pure[R](x: R): Wrap[R] = NonEmptyWrap(x)

    def flatMap[R](f: A => Wrap[R]): Wrap[R] = this match {
      case NonEmptyWrap(_) => f(this.get)
      case EmptyWrap => EmptyWrap
    }

    // HINT: map можно реализовать через pure и flatMap
    def map[R](f: A => R): Wrap[R] = this match {
      case NonEmptyWrap(_) => this.flatMap(f andThen pure)
      case EmptyWrap => EmptyWrap
    }


    def filter(f: A => Boolean): Wrap[A] =
      this match {
        case NonEmptyWrap(x) if f(x) => this
        case _                       => EmptyWrap
      }

    def withFilter(f: A => Boolean): Wrap[A] = {
      val value = this.get
      class NonEmptyWrapWithFilter(p: A => Boolean)
          extends NonEmptyWrap[A](value) {
        override def map[B](f: A => B): Wrap[B] = self filter p map f
        override def flatMap[B](f: A => Wrap[B]): Wrap[B] =
          self filter p flatMap f
        override def withFilter(q: A => Boolean): NonEmptyWrapWithFilter =
          new NonEmptyWrapWithFilter(x => p(x) && q(x))
      }
      new NonEmptyWrapWithFilter(f)
    }

  }

  object Wrap {
    def empty[R]: Wrap[R] = EmptyWrap
  }

  case class NonEmptyWrap[A](result: A) extends Wrap[A] {
    override def get: A = result
  } // pure

  case object EmptyWrap extends Wrap[Nothing] {
    override def get: Nothing =
      throw new NoSuchElementException("EmptyWrap.get")
  } // bottom, null element

}
