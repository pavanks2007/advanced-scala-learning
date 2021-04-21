package lectures.part2

import scala.annotation.tailrec

/**
 * Created by Pavan.
 */
trait MySet[A] extends (A => Boolean) {
  /*
    EXERCISE #1 - implement a functional set
   */
  def apply(elem: A): Boolean = contains(elem)
  def contains(elem:A): Boolean
  def +(elem:A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  /*
    EXERCISE #2
    - removing an element
    - difference with another set
    - intersection with another set
   */
  def -(elem:A): MySet[A] // remove
  def --(anotherSet: MySet[A]): MySet[A] // difference
  def &(anotherSet: MySet[A]): MySet[A] // intersection

  // EXERCISE #3 - implement a unary_! = NEGATION of a set
  def unary_! : MySet[A]
}

class EmptySet[A] extends MySet[A] {

  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]

  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def foreach(f: A => Unit): Unit = ()

  override def -(elem: A): MySet[A] = this

  override def --(anotherSet: MySet[A]): MySet[A] = this

  override def &(anotherSet: MySet[A]): MySet[A] = this

  override def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)

}

//class AllInclusiveSet[A] extends MySet[A] {
//  override def contains(elem: A): Boolean = true
//  override def +(elem: A): MySet[A] = this
//  override def ++(anotherSet: MySet[A]): MySet[A] = this
//  override def flatMap[B](f: A => MySet[B]): MySet[B] = ???      // Cannot be implemented easily
//  override def filter(predicate: A => Boolean): MySet[A] = ???   // Cannot be implemented easily
//  override def foreach(f: A => Unit): Unit = ???                 // Cannot be implemented easily
//  override def -(elem: A): MySet[A] = ???                        // Cannot be implemented easily
//  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
//  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)
//  override def unary_! : MySet[A] = new EmptySet[A]
//}

// all elements of type A which satisfy a property
// { x in A | property(x) }
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {

  override def contains(elem: A): Boolean = property(elem)

  // {x in A | property(x) } + element = { x in A | property(x) || x == element }
  override def +(elem: A): MySet[A] = new PropertyBasedSet[A](x => property(x) || x == elem)

  // {x in A | property(x) } ++ set => { x in A | property(x) || set contains x}
  override def ++(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  override def map[B](f: A => B): MySet[B] = politelyFail
  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  override def foreach(f: A => Unit): Unit = politelyFail

  override def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && predicate(x))

  override def -(elem: A): MySet[A] = filter(_ != elem)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")

}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {

  override def contains(elem: A): Boolean = elem == head || tail.contains(elem)

  override def +(elem: A): MySet[A] =
    if(this contains elem) this
    else new NonEmptySet[A](elem, this)

  /*
    [1 2 3] ++ [4 5 6] =
    [2 3] ++ [4 5 6] + 1 =
    [3] ++ [4 5 6] + 1 + 2 =
    [] ++ [4 5 6] + 1 + 2 + 3 =
    [4 5 6 1 2 3] // ordering does not matter
   */
  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head

  override def map[B](f: A => B): MySet[B] = (tail map f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)

  override def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if(predicate(head)) filteredTail + head
    else filteredTail
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  override def -(elem: A): MySet[A] =
    if (head == elem) tail
    else tail - elem + head

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet) // intersection = filtering!

  override def unary_! : MySet[A] = new PropertyBasedSet[A](!this.contains(_))

}

object MySet {

  /*
  val s = MySet(1, 2, 3) = buildSet(seq(1, 2, 3), [])
  = buildSet(seq(2,3), [] + 1)
  = buildSet(seq(3), [1] + 2)
  = buildSet(seq(), [1, 2] + 3)
  = [1, 2, 3]
   */
  def apply[A](values: A*):MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values.toSeq, new EmptySet[A])
  }

}

object MySetPlayground extends App {

  val s = MySet(1,2,3,4)
//  s + 5 ++ MySet(-1, -2) + 3 map (_ * 10) flatMap (x => MySet(x, 10 * x)) filter (_ % 2 == 0) foreach println

  val negative = !s // s.unary_! = all the naturals not equals to 1,2,3,4
  assert(negative(2) == false)
  assert(negative(5) == true)

  val negativeEven = negative.filter(_ % 2 == 0)
  assert(negativeEven(5) == false)

  val negativeEven5 = negativeEven + 5
  assert(negativeEven5(5) == true)

}
