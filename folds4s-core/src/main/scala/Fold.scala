package folds4s

import scalaz._

trait Fold[A, B] {
  type I
  def step(x: I, a: A): I
  def initial: I
  def extract(x: I): B

  def apply[F[_]](fa: F[A])(implicit F: Foldable[F]): B =
    extract(F.foldLeft(fa, initial)(step(_, _)))

  def zip[C](b: Fold[A, C]): Fold[A, (B, C)] =
    Fold.FoldInstances[A].tuple2(this, b)

  def generalise[M[_]](implicit M: Applicative[M]): FoldM[M, A, B] =
    FoldM(
      (a: I, b) => M.point(step(a, b)),
      M.point(initial),
      (a: I) => M.point(extract(a))
    )
}

object Fold {
  def apply[A, B, I](stepImpl: (I, A) => I, initialImpl: I, extractImpl: I => B): Fold[A, B] = {
    type Impl =
      I
    new Fold[A, B] {
      type I =
        Impl
      def step(i: I, a: A): I =
        stepImpl(i, a)
      val initial: I =
        initialImpl
      def extract(i: I): B =
        extractImpl(i)
    }
  }

  def simple[A, B](stepImpl: (B, A) => B, initialImpl: B): Fold[A, B] =
    apply(
      stepImpl,
      initialImpl,
      identity[B]
    )

  implicit def FoldInstances[X]: Comonad[({type l[a]=Fold[X, a]})#l] with Applicative[({type l[a]=Fold[X, a]})#l] =
    new Comonad[({type l[a]=Fold[X, a]})#l] with Applicative[({type l[a]=Fold[X, a]})#l] {
      def point[A](a: => A): Fold[X, A] =
        new Fold[X, A] {
          type I =
            Unit
          def step(u: Unit, x: X): Unit =
            ()
          val initial: Unit =
            ()
          def extract(u: Unit): A =
            a
        }
      def ap[A, B](fa: => Fold[X, A])(f: => Fold[X, A => B]): Fold[X, B] = {
        val _fa: Fold[X, A] =
          fa
        val _f: Fold[X, A => B] =
          f
        new Fold[X, B] {
          type I =
            (_f.I, _fa.I)
          def step(i: (_f.I, _fa.I), x: X): (_f.I, _fa.I) =
            (_f.step(i._1, x), _fa.step(i._2, x))
          val initial: (_f.I, _fa.I) =
            (_f.initial, _fa.initial)
          def extract(i: (_f.I, _fa.I)): B =
            _f.extract(i._1)(_fa.extract(i._2))
        }
      }
      def cobind[A, B](fa: Fold[X, A])(f: Fold[X, A] => B): Fold[X, B] =
        new Fold[X, B] {
          type I =
            fa.I
          def step(i: fa.I, x: X): fa.I =
            fa.step(i, x)
          val initial: fa.I =
            fa.initial
          def extract(i: fa.I): B =
            f(
              new Fold[X, A] {
                type I =
                  fa.I
                def step(i: fa.I, x: X): fa.I =
                  fa.step(i, x)
                val initial: fa.I =
                  i
                def extract(i: fa.I): A =
                  fa.extract(i)
              }
            )
        }

      def copoint[A](p: Fold[X, A]): A =
        p.extract(p.initial)
    }

  implicit val FoldProfunctor: Profunctor[Fold] =
    new Profunctor[Fold] {
      def mapfst[A, B, C](fab: Fold[A, B])(f: C => A): Fold[C, B] =
        new Fold[C, B] {
          type I =
            fab.I
          def step(x: fab.I, c: C): fab.I =
            fab.step(x, f(c))
          val initial: fab.I =
            fab.initial
          def extract(i: fab.I): B =
            fab.extract(i)
        }
      def mapsnd[A, B, C](fab: Fold[A, B])(f: B => C): Fold[A, C] =
        new Fold[A, C] {
          type I =
            fab.I
          def step(x: fab.I, a: A): fab.I =
            fab.step(x, a)
          val initial: fab.I =
            fab.initial
          def extract(i: fab.I): C =
            f(fab.extract(i))
        }
    }


  def concat[A](implicit A: Monoid[A]): Fold[A, A] =
    Fold.simple(
      A.append(_, _),
      A.zero
    )

  def foldMap[A, B, W](f: A => W, g: W => B)(implicit W: Monoid[W]): Fold[A, B] =
    Fold(
      (x: W, a) => W.append(x, f(a)),
      W.zero,
      g
    )

  def head[A]: Fold[A, Maybe[A]] =
    _Fold1((a, _) => a)

  def last[A]: Fold[A, Maybe[A]] =
    _Fold1((_, a) => a)

  def lastOr[A](a: A): Fold[A, A] =
    Fold.simple(
      (_, b) => b,
      a
    )

  def isEmpty[A]: Fold[A, Boolean] =
    Fold.simple(
      (_, _) => false,
      true
    )

  def length[A]: Fold[A, Int] =
    Fold.simple(
      (n, _) => n + 1,
      0
    )

  val and: Fold[Boolean, Boolean] =
    Fold.simple(
      _ && _,
      true
    )

  val or: Fold[Boolean, Boolean] =
    Fold.simple(
      _ || _,
      false
    )

  val sum: Fold[Int, Int] =
    Fold.simple(
      _ + _,
      0
    )

  def all[A](f: A => Boolean): Fold[A, Boolean] =
    Fold.simple(
      _ && f(_),
      true
    )

  def any[A](f: A => Boolean): Fold[A, Boolean] =
    Fold.simple(
      _ || f(_),
      false
    )

  val product: Fold[Int, Int] =
    Fold.simple(
      _ * _,
      1
    )

  def _Fold1[A](to: (A, A) => A): Fold[A, Maybe[A]] =
    Fold.simple(
      (x, a) => Maybe.just(x.cata(to(_, a), a)),
      Maybe.empty
    )
}
