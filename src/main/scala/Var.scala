package com.github.hexx.bound

sealed trait Var[+A, B] {
  def unvar[R](f: B => R, g: A => R): R
}

case class Bound[+A, B](b: B) extends Var[A, B] {
  def unvar[R](f: B => R, g: A => R) = f(b)
}

case class Free[+A, B](a: A) extends Var[A, B] {
  def unvar[R](f: B => R, g: A => R) = g(a)
}
