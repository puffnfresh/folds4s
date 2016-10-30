package folds4s

import scalaz.{ Monad, Foldable }

trait FoldM[M[_], A, B] {
  type I
  def step(x: I, a: A): M[I]
  def initial: M[I]
  def extract(x: I): M[B]

  def apply[F[_]](fa: F[A])(implicit F: Foldable[F], M: Monad[M]): M[B] =
    M.bind(initial) { x0 =>
      M.bind(F.foldLeftM(fa, x0)(step(_, _)))(extract)
    }
}

object FoldM {
  def apply[M[_], A, B, I](stepImpl: (I, A) => M[I], initialImpl: M[I], extractImpl: I => M[B]): FoldM[M, A, B] = {
    type Impl =
      I
    new FoldM[M, A, B] {
      type I =
        Impl
      def step(i: I, a: A): M[I] =
        stepImpl(i, a)
      val initial: M[I] =
        initialImpl
      def extract(i: I): M[B] =
        extractImpl(i)
    }
  }
}
