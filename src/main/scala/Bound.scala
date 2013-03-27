package com.github.hexx.bound

import scalaz._
import Scalaz._

trait Bound[F[_[_], _]] {
  def bind[G[_]: Monad, A, B](m: F[G, A])(f: A => G[B]): F[G, B]
}

trait BoundOps[F[_[_], _], G[_], A] {
  def self: F[G, A]
  implicit def F: Bound[F]

  def >>>=[B](f: A => G[B])(implicit G: Monad[G]): F[G, B] = F.bind(self)(f)
}
