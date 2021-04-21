

object AdvancedPatternMatching extends App{

  val numbers = List(1)

  class Person(val name:String, val age:Int)

  object Person{
    def unapply(person: Person): Option[(String, Int)] = {
      if(person.age < 21) None
      else Some((person.name, person.age))
    }

    def unapply(age:Int): Option[String] = {
      Some(if (age < 21) "minor" else "major")
    }
  }

  val pavan = new Person("Pavan", 24)
  val greeting = pavan match {
    case Person(name, age) => s"Hi, my name is $name and I am $age years old."
  }
  println(greeting)

  val legalStatus = pavan.age match {
    case Person(status) => s"My legal status is $status"
  }
  println(legalStatus)

  object even {
    def unapply(arg:Int): Boolean = arg % 2 == 0
  }

  object singleDigit {
    def unapply(arg:Int): Boolean = arg > -10 && arg < 10
  }

  val n:Int = 8
  val mathProperty: String = n match {
    case singleDigit() => "single digit"
    case even() => "even number"
    case _ => "no property"
  }
  println(mathProperty)

  //infix patterns
  case class Or[A, B](a: A, b: B)
  val either = Or(2, "two")
  val humanDescription = either match {
    case number Or string => s"$number is written as $string"
  }
  println(humanDescription)

  //decomposing sequences
  val vararg = numbers match {
    case List(1, _*) => "starting with 1"
  }
  println(vararg)

  //
}
