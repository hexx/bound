package com.github.hexx.bound.succthewholething

import scalaz._, Scalaz._

sealed trait Incr[+A]
case class Succ[+A](a: A) extends Incr[A]
case object Zero extends Incr[Nothing]

case class Scope[F[+_], +A](fa: F[Incr[F[A]]])

object Incr {
  implicit val incrMonad = new Monad[Incr] {
    def point[A](a: => A) = Succ(a)
    def bind[A, B](i: Incr[A])(f: A => Incr[B]) = i match {
      case Zero    => Zero
      case Succ(a) => f(a)
    }
  }

  implicit val incrTraverse = new Traverse[Incr] {
    def traverseImpl[F[_], A, B](i: Incr[A])(f: A => F[B])(implicit F: Applicative[F]) = i match {
      case Zero    => F.point(Zero)
      case Succ(a) => f(a) map (Succ(_))
    }
  }
}

object Scope {
  implicit def scopeMonad[F[+_]](implicit F: Monad[F]) = new Monad[({type λ[α] = Scope[F, α]})#λ] {
    def point[A](a: => A) = Scope(F.point(Succ(F.point(a))))
    def bind[A, B](s: Scope[F, A])(f: A => Scope[F, B]): Scope[F, B] =
      Scope(s.fa.flatMap {
        case Zero => F.point(Zero)
        case Succ(a) => a >>= (f andThen (_.fa))
      })
  }

  implicit val scopeMonadTrans = new MonadTrans[Scope] {
    def liftM[F[+_], A](fa: F[A])(implicit F: Monad[F]) = Scope(fa.map(a => Succ(F.point(a))))
    implicit def apply[F[+_] : Monad] = scopeMonad
  }

  def abstrakt[F[+_], A: Equal](x: A, e: F[A])(implicit F: Monad[F]): Scope[F, A] = {
    def k(y: A): Incr[F[A]] = if (x === y) Zero else Succ(F.point(y))
    Scope(e map k)
  }

  def instantiate[F[+_]: Monad, A](r: F[A], s: Scope[F, A]): F[A] = {
    s.fa flatMap {
      case Zero    => r
      case Succ(a) => a
    }
  }
}
