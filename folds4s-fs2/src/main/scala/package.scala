package folds4s.fs2

import folds4s.{ Fold, FoldM }
import fs2._
import fs2.util.{ Catchable, Monad, Sub1 }

object `package` {
  private def foldLeftM[M[_], A, B](l: Chunk[A], b: B)(f: (B, A) => M[B])(implicit M: Monad[M]): M[B] = {
    def go(h: A, t: Chunk[A], m: M[B]): M[B] = {
      val r = M.flatMap(m)(f(_, h))
      t.uncons.fold(r) {
        case (h1, t1) =>
          go(h1, t1, r)
      }
    }
    val mb = M.pure(b)
    l.uncons.fold(mb) {
      case (h, t) =>
        go(h, t, mb)
    }
  }

  def toPipe[M[_]: Monad, A, B](f: FoldM[M, A, B]): Pipe[M, A, B] =
    s => s.pull(fromHandle(f, _))

  def fromHandle[M[_]: Monad, A, B](f: FoldM[M, A, B], h: Handle[M, A]): Pull[M, Nothing, B] = {
    def pure[N[_], C](c: C): Pull[N, Nothing, C] =
      Sub1.substPull[Pure, N, Nothing, C](Pull.pure(c))
    def go(h1: Handle[M, A], i: f.I): Pull[M, Nothing, f.I] =
      h1.await.optional.flatMap(_.fold(pure[M, f.I](i)) {
        case (c, h2) =>
          Pull.eval(foldLeftM(c, i)(f.step(_, _))).flatMap(go(h2, _))
      })
    Pull.eval(f.initial).flatMap(go(h, _)).flatMap(i => Pull.eval(f.extract(i)))
  }

  def fold[M[_], A, B](s: Stream[M, A], f: Fold[A, B])(implicit C: Catchable[M]): M[B] =
    C.map(s.runFold(f.initial)(f.step(_, _)))(f.extract)

  def foldM[M[_], A, B](s: Stream[M, A], f: FoldM[M, A, B])(implicit C: Catchable[M]): M[B] =
    C.flatMap(s.through(toPipe(f)).runLast)(_.fold(C.flatMap(f.initial)(f.extract))(C.pure))
}
