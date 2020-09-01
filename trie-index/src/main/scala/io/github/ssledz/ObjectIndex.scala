package io.github.ssledz

import io.github.ssledz.ObjectIndex.KeyGenerator
import HList._

case class Key[H <: HList](value: H)

object Key {

  def apply[A, B, C](a: A, b: B, c: C): Key[A #:: B #:: C #:: HNil] = new Key(a #:: b #:: c #:: HNil)

}

trait ObjectIndex[K, V] {
  def get(key: K): Seq[V]
}

object ObjectIndex {

  def empty[K, V]: ObjectIndex[K, V] = _ => Seq.empty

  type KeyGenerator[V, K] = V => Iterable[K]

  def apply[H <: HList, V](xs: Seq[V])(implicit gen: KeyGenerator[V, Key[H]]): ObjectIndex[Key[H], V] =
    if (xs.isEmpty) empty[Key[H], V] else TrieObjectIndex[H, V](xs)(???)

}

private case class TrieObjectIndex[H <: HList, V]() extends ObjectIndex[Key[H], V] {

  def get(key: Key[H]): Seq[V] = {

    ???
  }
}

private object TrieObjectIndex {

  sealed trait Trie

  case class Node[K](underlying: collection.Map[K, Trie]) extends Trie

  case class Leaf[V](values: Seq[V]) extends Trie

  def apply[H <: HList, V](values: Seq[V])(gens: List[KeyGenerator[V, _]]): TrieObjectIndex[H, V] = gens match {
    case h :: t =>

      val key2value = for {
        value <- values
        key <- h(value)
      } yield (key, value)

      ???

    case Nil => ???
  }

}

object TestApp extends App {

  type KeyType = Key[Int #:: String #:: Double #:: HNil]

  implicit val keyGenerator: KeyGenerator[String, KeyType] = str => Iterable(Key(str.length, str, str.length * 1.1))

  val index = ObjectIndex(Seq("one", "two", "three"))

  val objs = index.get(Key(1, "", 1.2))

  //  index.get(Key(1, 1, 1.2))

}