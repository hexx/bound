package com.github.hexx.bound

import scalaz._
import Scalaz._

case class Scope[F[_], A, B](s: F[Var[F[A], B]]) {
  def unscope: F[Var[F[A], B]] = s
}

object Scope {
  def abstract_[F[_], A, B](f: A => Option[B], e: F[A])(implicit F: Monad[F]): Scope[F, A, B] = {
    def k(y: A): Var[F[A], B] = f(y).map(Bound(_)) getOrElse Free(F.point(y))
    Scope(e.map(k(_)))
  }

  def abstract_1[F[_]: Monad, A: Equal](a: A, fa: F[A]): Scope[F, A, Unit] =
    abstract_((b: A) => if (a === b) Some(()) else None, fa)

  def instantiate[F[_]: Monad, A, B](k: B => F[A], s: Scope[F, A, B]): F[A] = {
    s.unscope.flatMap(_ match {
      case Bound(b) => k(b)
      case Free(a) => a
    })
  }

  def instantiate1[F[_]: Monad, A, B](e: F[A], s: Scope[F, A, B]) = instantiate((_: B) => e, s)
}
