package io.github.ssledz

import io.github.ssledz.HList.{HNil, _}

import scala.annotation.tailrec
import scala.language.implicitConversions

sealed trait HList extends Product with Serializable {

  def #::[H](h: H): H #:: this.type = new #::(h, this)

  def toList: List[_] = {
    @tailrec
    def go(acc: List[_], xs: HList): List[_] = xs match {
      case h #:: t => go(h :: acc, t)
      case HNil => acc
    }

    go(List.empty, this)
  }

  def headOption: Option[Any]

  def tail: HList

}

object HList {

  final case class #::[+H, +T <: HList](private[ssledz] val head: H, tail: T) extends HList {
    def headOption: Option[H] = Some(head)
  }

  sealed trait HNil extends HList {
    def headOption: Option[Any] = None

    def tail: HList = HNil
  }

  case object HNil extends HNil

}


object HListTestApp extends App {

  val ys = 1 #:: "sss" #:: HNil
  val xs: Int #:: String #:: HNil = ys

  //  println(xs)

  val key: Int #:: String #:: HNil = 1 #:: "key2" #:: HNil


  val key1: String #:: HNil = key.tail
  val key2: HNil = key.tail.tail

  println(key.toList)
  println(key.headOption)
  println(key.tail.headOption)
  println(key.tail.tail.headOption)
  println(key.tail.tail.tail.headOption)

}

