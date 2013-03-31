package com.github.hexx.bound.justdontsucc

import scalaz._, Scalaz._

sealed trait Incr[+A]
case class Succ[+A](a: A) extends Incr[A]
case object Zero extends Incr[Nothing]

case class Scope[F[+_], +A](fa: F[Incr[A]])

sealed trait Exp[+A]
case class Var[+A](a: A) extends Exp[A]
case class App[+A](fun: Exp[A], arg: Exp[A]) extends Exp[A]
case class Lam[+A](s: Scope[Exp, A]) extends Exp[A]

object Exp {
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

  implicit def scopeMonad[F[+_]](implicit F: Monad[F]) = new Monad[({type λ[α] = Scope[F, α]})#λ] {
    def point[A](a: => A) = Scope(F.point(Succ(a)))
    def bind[A, B](s: Scope[F, A])(f: A => Scope[F, B]): Scope[F, B] =
      Scope(s.fa.flatMap {
        case Zero => F.point(Zero)
        case Succ(a) => f(a).fa
      })
  }

  implicit def scopeTraverse[F[+_]: Traverse] = new Traverse[({type λ[α] = Scope[F,α]})#λ] {
    def traverseImpl[G[+_], A, B](s: Scope[F, A])(f: A => G[B])(implicit G: Applicative[G]): G[Scope[F, B]] = s.fa traverse (_ traverse f) map (Scope(_))
  }

  implicit def scopeMonadTrans = new MonadTrans[Scope] {
    def liftM[F[+_]: Monad, A](a: F[A]) = Scope(a.map(Succ(_)))
    implicit def apply[F[+_] : Monad] = scopeMonad
  }

  implicit def expShow[A] = Show.showA[Exp[A]]

  implicit val expMonad: Monad[Exp] = new Monad[Exp] {
    def point[A](a: => A) = Var(a)

    def bind[A, B](e: Exp[A])(f: A => Exp[B]): Exp[B] = e match {
      case Var(a)        => f(a)
      case App(fun, arg) => App(bind(fun)(f), bind(arg)(f))
      case Lam(s)        => Lam(s >>= (f andThen (MonadTrans[Scope].liftM(_))))
    }
  }

  implicit val expFoldable: Foldable[Exp] = new Foldable[Exp] with Foldable.FromFoldMap[Exp] {
    def foldMap[A, B](e: Exp[A])(f: A => B)(implicit F: Monoid[B]): B = e match {
      case Var(a)        => f(a)
      case App(fun, arg) => foldMap(fun)(f) |+| foldMap(arg)(f)
      case Lam(s)        => s foldMap f
    }
  }

  implicit val expTraverse: Traverse[Exp] = new Traverse[Exp] {
    def traverseImpl[F[+_], A, B](e: Exp[A])(f: A => F[B])(implicit F: Applicative[F]): F[Exp[B]] = e match {
      case Var(a)        => f(a) map (Var(_))
      case App(fun, arg) => ^(traverseImpl(fun)(f), traverseImpl(arg)(f))(App(_, _))
      case Lam(s)        => s traverse f map (Lam(_))
    }
  }

  def example = {
    // \x.x
    Lam(Scope(Var(Zero)))
    // \x.\y.x
    Lam(Scope(Lam(Scope(Var(Succ(Zero))))))
    // \x.\y.x y z
    Lam(Scope(Lam(Scope(App(App(Var(Succ(Zero)), Var(Zero)), Var(Succ(Succ("z"))))))))
  }
}
