package com.github.hexx.bound.notanum

import scalaz._, Scalaz._

case class Scope[F[+_], +A](fa: F[A])

sealed trait Exp[+A]
case class Free[+A](a: A) extends Exp[A]
case class Bound[+A](i: Int) extends Exp[A]
case class App[+A](e1: Exp[A], e2: Exp[A]) extends Exp[A]
case class Lam[+A](s: Scope[Exp, A]) extends Exp[A]

object Exp {
  implicit def expShow[A] = Show.showA[Exp[A]]

  implicit val expMonad = new Monad[Exp] {
    def point[A](a: => A) = Free(a)

    def bind[A,B](e: Exp[A])(f: A => Exp[B]): Exp[B] = e match {
      case Free(a)     => f(a)
      case Bound(i)    => Bound(i)
      case App(e1, e2) => App(bind(e1)(f), bind(e2)(f))
      case Lam(s)      => Lam(Scope(bind(s.fa)(f)))
    }
  }

  implicit val expFoldable = new Foldable[Exp] with Foldable.FromFoldMap[Exp] {
    def foldMap[A, B](e: Exp[A])(f: A => B)(implicit F: Monoid[B]): B = e match {
      case Free(a)     => f(a)
      case Bound(i)    => F.zero
      case App(e1, e2) => foldMap(e1)(f) |+| foldMap(e2)(f)
      case Lam(s)      => foldMap(s.fa)(f)
    }
  }

  implicit val expTraverse = new Traverse[Exp] {
    def traverseImpl[F[_], A, B](e: Exp[A])(f: A => F[B])(implicit F: Applicative[F]): F[Exp[B]] = e match {
      case Free(a)     => f(a) map (Free(_))
      case Bound(i)    => F.point(Bound(i))
      case App(e1, e2) => ^(traverseImpl(e1)(f), traverseImpl(e2)(f))(App(_, _))
      case Lam(s)      => traverseImpl(s.fa)(f) map (e => Lam(Scope(e)))
    }
  }

  def abstrakt[A: Equal](me: A, e: Exp[A]): Scope[Exp, A] = {
    def letmeB(i: Int, e0: Exp[A]): Exp[A] = e0 match {
      case Free(you) => if (me === you) Bound(i) else Free(you)
      case Bound(i) => Bound(i)
      case App(fun, arg) => App(letmeB(i, fun), letmeB(i, arg))
      case Lam(Scope(body)) => Lam(Scope(letmeB(i + 1, body)))
    }
    Scope(letmeB(0, e))
  }

  def instantiate[A](what: Exp[A], s: Scope[Exp, A]): Exp[A] = {
    def whatsB(i: Int, e0: Exp[A]): Exp[A] = e0 match {
      case Free(you)     => Free(you)
      case Bound(j)      => if (i == j) what else Bound(j)
      case App(fun, arg) => App(whatsB(i, fun), whatsB(i, arg))
      case Lam(s)        => Lam(Scope(whatsB(i + 1, s.fa)))
    }
    whatsB(0, s.fa)
  }

  def lam[A: Equal](v: A, b: Exp[A]): Exp[A] = Lam(abstrakt(v, b))

  def whnf[A](e: Exp[A]): Exp[A] = e match {
    case App(f,a) => whnf(f) match {
      case Lam(b) => whnf(instantiate(a, b))
      case g      => App(g, a)
    }
    case e => e
  }
}
