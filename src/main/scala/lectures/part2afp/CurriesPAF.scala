package lectures.part2afp

object CurriesPAF extends App {

  // curried functions
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3) // Int => Int => 3 + y
  println(add3(5))
  println(superAdder(3)(5))

  // METHOD
  def curriedAdder(x: Int)(y: Int): Int = x + y // curried method

  val add4: Int => Int = curriedAdder(4) // lifting = ETA-EXPANSION
  // val add4_1 = curriedAdder(4) // throws error

  // functions != methods (JVM limitation)
  def inc(x:Int): Int = x + 1
  List(1,2,3).map(inc) // ETA-EXPANSION equals to List(1,2,3).map(x => inc(x))

  // Partial function applications
  val add5 = curriedAdder(5)(_) // Int => Int

  // EXERCISE
  val simpleAddFunction = (x: Int, y:Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  //add7: Int => Int = y => 7 + y
  val add7 = (x: Int) => simpleAddFunction(7, x)
  val add7_2 = simpleAddFunction.curried(7)

  val add7_3 = curriedAddMethod(7) _  // PAR
  val add7_4 = curriedAddMethod(7)(_) // PAR = alternate syntax

  val add7_5 = simpleAddMethod(7, _: Int) // alternate syntax  for turning methods into function values
               // y  => simpleAddMethod(7, y)
  val add7_6 = simpleAddFunction(7, _: Int)

  // underscores are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c
  val insertName = concatenator("Hello, I'm ", _: String, ", how are you?") // x: String => concatenator(hello, x, how are you)
  println(insertName("Pavan"))

  val fillInTheBlanks = concatenator("Hello, ", _: String, _: String) // (x, y) => concatenator("Hello, ", x, y)
  println(fillInTheBlanks("Pavan", "Scala is awesome!"))

  // EXERCISES
  /*
    1.  Process a list of numbers and return their string representations with different formats
        Use the %4.2f, %8.6f and %14.12f with a curried formatter function.
   */
  def curriedFormatter(s: String)(number: Double): String = s.format(number)
  val numbers = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)

  val simpleFormat = curriedFormatter("%4.2f") _ // lift
  val seriousFormat = curriedFormatter("%8.6f") _ // lift
  val preciseFormat = curriedFormatter("%14.12f") _ // lift

  println(numbers.map(simpleFormat))
  println(numbers.map(seriousFormat))
  println(numbers.map(preciseFormat))
  println(numbers.map(curriedFormatter("%14.12f"))) // compiler does sweet eta-expansion for us

  /*
    2.  difference between
        - functions vs methods
        - parameters: by-name vs 0-lambda
   */
  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 42
  /*
    calling byName and byFunction
    - int
    - method
    - parenMethod
    - lambda
    - PAF
   */
  byName(23) // ok
  byName(method) // ok
  byName(parenMethod()) //ok
  byName(parenMethod) //ok but beware ==> byName(parenMethod())
  // byName(() => 42) // not ok
  byName((() => 42)()) // ok
  // byName(parenMethod _) // not ok

  // byFunction(42) // not ok
  // byFunction(method) // not ok!!!!! does not do ETA-expansion!
  byFunction(parenMethod) // ok
  byFunction(() => 42) // ok
  byFunction(parenMethod _) // ok ===> byFunction(parenMethod)

}
