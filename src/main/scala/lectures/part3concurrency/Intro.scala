package lectures.part3concurrency

import java.util.concurrent.Executors

object Intro extends App {

  /*
    interface Runnable {
      public void run()
    }
   */
  // JVM threads
  val runnable = new Runnable {
    override def run(): Unit = println("Running in parallel")
  }
  val aThread = new Thread(runnable)

  //  aThread.start() // fives the signal to the JVM to start a JVM thread on top of an OS thread
  //  // create a JVM thread => OS thread
  //  runnable.run() // doesn't do anything in parallel!
  //  aThread.join() // blocks until aThread finishes running

  //  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  //  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))
  //  threadHello.start()
  //  threadGoodbye.start()
  //  different runs produce different results!

  // executors
  val pool = Executors.newFixedThreadPool(10)
  //  pool.execute(() => println("something in the thread pool"))
  //
  //  pool.execute(() => {
  //    Thread.sleep(1000)
  //    println("done after 1 second")
  //  })
  //
  //  pool.execute(() => {
  //    Thread.sleep(1000)
  //    println("almost done")
  //    Thread.sleep(1000)
  //    println("done after 2 seconds")
  //  })

  //  pool.shutdown()
  //  pool.execute(() => println("should not appear")) // throw an exception in the calling thread

  //  pool.shutdownNow()
  //  println(pool.isShutdown) // true

  def runInParallel = {
    var x = 0

    val thread1 = new Thread(() => {
      x = 1
    })

    val thread2 = new Thread(() => {
      x = 2
    })

    thread1.start()
    thread2.start()
    println(x)
  }

  //  for(_ <- 1 to 100) runInParallel // race condition

  class BankAccount(var amount: Int) {
    override def toString(): String = "" + amount
  }

  def buy(account: BankAccount, thing: String, price: Int) = {
    account.amount -= price
//    println("I've bought " + thing)
//    println("My account is now " + account)
  }

//  for (_ <- 1 to 1000) {
//    val account = new BankAccount(50000)
//    val thread1 = new Thread(() => buy(account, "shoes", 3000))
//    val thread2 = new Thread(() => buy(account, "iPhone", 4000))
//
//    thread1.start()
//    thread2.start()
//    Thread.sleep(100)
//    if ( account.amount != 43000) println("AHA: " + account.amount)
//    println()
//  }

  // Option #1: use synchronized()
  def buySafe(account: BankAccount, thing: String, price: Int) = {
    account.synchronized({
      account.amount -= price
      println("I've bought " + thing)
      println("My account is now " + account)
    })
  }

  // Option #2: use @volatile

  /**
   * Exercises
   *
   * 1) Construct 50 "inception" threads
   *      Thread1 -> Thread2 -> Thread3 -> ...
   *      println("hello from thread #3")
   *      in REVERSE ORDER
   */
  def inceptionThreads(maxThreads: Int, i: Int = 1): Thread = new Thread(() => {
    if(i < maxThreads) {
      val newThread = inceptionThreads(maxThreads, i + 1)
      newThread.start()
      newThread.join()
    }
    println(s"Hello from thread #$i")
  })

//  inceptionThreads(50).start()

  /*
    2)
      2.1) What is the BIGGEST value possible for x? 100
      2.2) What is the SMALLEST value possible for x? 1
   */
  //  var x = 0
  //  val threads = (1 to 100).map(_ => new Thread(() => x += 1))
  //  threads.foreach(_.start())
  //  threads.foreach(_.join())
  //  println(x)

  /*
    3) sleep fallacy
       What's the value of the message? almost always "Scala is awesome"
       Is it guaranteed? NO!
   */
  var message = ""
  val awesomeThread = new Thread(() => {
    Thread.sleep(1000)
    message = "Scala is awesome"
  })
  message = "Scala sucks"
  awesomeThread.start()
  Thread.sleep(1001)
  // synchronized does NOT work as it only works with concurrent changes
  awesomeThread.join() // fix!
  println(message)
}

