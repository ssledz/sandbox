package io.github.ssledz

object RecursionApp extends App {

  sealed trait Nat

  trait _0 extends Nat

  trait Succ[N <: Nat] extends Nat

  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]

  class RepNat[T <: Nat](val value: Int)

  def rep[T <: Nat](implicit r: RepNat[T]) = r.value

  implicit val repZero = new RepNat[_0](0)

  implicit def repSucc[A <: Nat, B <: Nat](implicit ev: A <:< Succ[B], r: RepNat[B]): RepNat[A] = {
    new RepNat[A](r.value + 1)
  }

  println(rep[_0])
  println(rep[_1])
  println(rep[_2])
  println(rep[_3])
  println(rep[_4])


}

object HListRecursion extends App {

  import HList._

  case class Acc[T <: HList](f: T => (HList, List[Any]))

  def rep[T <: HList](hl: T)(implicit acc: Acc[T]): List[Any] = acc.f(hl)._2

  implicit def nil[T <: HNil]: Acc[T] = Acc(_ => (HNil, List.empty))

  implicit def recHList[TT <: HList, H, T <: HList](implicit ev: TT =:= (H #:: T), acc: Acc[T]): Acc[TT] = Acc[TT] { hl =>

    val (hhl, xs) = acc.f(hl.tail.asInstanceOf[T])


    (hhl, hl.head :: xs)
  }

  val xs = 1 #:: 2 #:: "Alla" #:: 1.3 #:: HNil

  println(rep(xs))

}