package lectures.part4implicits

object OrganizingImplicits extends App{

  implicit val reverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
//  implicit val normalOrdering: Ordering[Int] = Ordering.fromLessThan(_ < _)
  println(List(1,4,5,3,2).sorted)

  // scala.Predef
  /*
    Implicits:
    - val/var
    - object
    - accessor methods = defs with no parenthesis
   */

  case class Person(name: String, age: Int)

  val persons = List(
    Person("Steve", 30),
    Person("Amy", 22),
    Person("John", 66)
  )

  //  object SomeObject {
  //    implicit val alphabeticOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  //  } // Error as this scope is not checked for implicits

  //  object Person {
  //    implicit val alphabeticOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  //  } // Works as scope is in Person object

//  implicit val ageOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.age.compareTo(b.age) < 0)
//  println(persons.sorted)

  /*
    Implicit scope
    - normal scope = LOCAL SPACE
    - imported scope
    - companions of all types involved in the method signature
      - List
      - Ordering
      - all the types involved = A or any supertype
   */

  object AlphabeticNameOrdering {
    implicit val alphabeticOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  }

  object AgeOrdering {
    implicit val ageOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.age.compareTo(b.age) < 0)
  }

  import AlphabeticNameOrdering._
//  import AgeOrdering._
  println(persons.sorted)

  /*
    Exercise
    - totalPrice = most used (50%)
    - by unit count = 25%
    - by unit price = 25% (if used % was low, can be defined in local scope)
   */
  case class Purchase(nUnits: Int, unitPrice: Double)
  object Purchase {
    implicit val totalPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.nUnits * a.unitPrice < b.nUnits * b.unitPrice)
  }
  object UnitCountOrdering {
    implicit val unitCountOrdering: Ordering[Purchase] = Ordering.fromLessThan(_.nUnits < _.nUnits)
  }
  object UnitPriceOrdering {
    implicit val unitPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan(_.unitPrice < _.unitPrice)
  }

}
