// Monad interface
trait Monad[M[_]] {
  def pure[A](a: A): M[A]
  def bind[A, B](a: M[A], f: A => M[B]): M[B]
}

object Monad {
  // Monad instances
  val ValueMonad = new Monad[Function0] {
    def pure[A](a: A) = () => a
    def bind[A, B](a: Function0[A], f: A => Function0[B]) =
      f(a.apply)
  }

  val ListMonad = new Monad[List] {
    def pure[A](a: A) = List(a)
    def bind[A, B](a: List[A], f: A => List[B]) =
      a flatMap f
  }

  val OptionMonad = new Monad[Option] {
    def pure[A](a: A) = Some(a)
    def bind[A, B](a: Option[A], f: A => Option[B]) =
      a flatMap f
  }

  trait PA[T[_, _], A] {
    type Apply[B] = T[A, B]
  }

  def Function1Monad[X] = new Monad[PA[Function1, X]#Apply] {
    def pure[A](a: A) = _ => a
    def bind[A, B](a: X => A, f: A => X => B) =
      x => f(a(x))(x)
  }

  // Monad functions
  def sequence[M[_], A](as: List[M[A]], m: Monad[M]) =
    as.foldRight[M[List[A]]](m pure Nil)((h, t) =>
      m.bind(h, (a: A) => m.bind(t, (as: List[A]) => m pure (a :: as))))

  def join[M[_], A](a: M[M[A]], m: Monad[M]) = m.bind(a, (z: M[A]) => z)

  def replicate[M[_], A](n: Int, a: M[A], m: Monad[M]) =
    sequence(List.fill(n)(a), m)

  // etc. etc. (Monad functions)
}

// Monad demo
object Main {
  import Monad._

  def main(args: Array[String]) {
    println(sequence(List(List(1, 2, 3), List(4, 5, 6)), ListMonad))
    println(sequence(List(Some(7), Some(8)), OptionMonad))
    println(join(List(List(1, 2, 3), List(4, 5, 6)), ListMonad))
    println(join(Some(Some(7)), OptionMonad))
    println(replicate(3, "abc".toList, ListMonad) map (_.mkString))
    println(replicate(4, Some(8), OptionMonad))
  }
}

// scala-2.8.0.r19410-b20091106023416

/*
List(List(1, 4), List(1, 5), List(1, 6), List(2, 4), List(2, 5),
  List(2, 6), List(3, 4), List(3, 5), List(3, 6))
Some(List(7, 8))
List(1, 2, 3, 4, 5, 6)
Some(7)
List(aaa, aab, aac, aba, abb, abc, aca, acb, acc, baa,
  bab, bac, bba, bbb, bbc, bca, bcb, bcc, caa, cab, cac,
  cba, cbb, cbc, cca, ccb, ccc)
Some(List(8, 8, 8, 8))
*/
