package parralelism

import java.util.concurrent.{ExecutorService, Executors}

object Intro extends App {
  // JVM threads
  val aThread = new Thread(() => {
    println("Running in parralel")
  })

  aThread.start()
  // create a jvm thread which run on top of os thread
  // the thread is a different one than on which main method is executed

  aThread.join() // block until AThread finishes running

//  val threadHello = new Thread(() => {1 to 5 foreach(_ => println("hello"))})
//  val threadBye = new Thread(() => {1 to 5 foreach(_ => println("Good Bye"))})
//  threadHello.start()
//  threadBye.start()

  // thread runs in random order

  // executors -> Thread is very costly
  //so we create a thread pool instead of killing we reuse

  val pool: ExecutorService = Executors.newFixedThreadPool(10)
  pool.execute(() => {println("Something in thread pool")})

  pool.execute(() => {Thread.sleep(1000) ; println("Done after 1 second")})
  pool.execute(() => {Thread.sleep(1000) ; println("Almost done second")})

//  pool.shutdown() // shutdown all thread now no thread from pool will execute anything -> previously submitted task are executed
//  pool.shutdownNow() // it even stops the sleeping thread -> even previously submitted task throws exception

  def runInParralel = {
    var x = 0

    val thread1 = new Thread(() => x = 1)
    val thread2 = new Thread(() => x = 2)

    thread1.start()
    thread2.start()
    println(x)
  }

  for (_ <- 1 to 10) runInParralel
  // race condition -> two threads are trying to assign same memory space at the same time

  class BankAccount(@volatile var amount: Int) {
    override def toString: String = "" + amount
  }

  def buy(account: BankAccount, thing: String, price: Int) = {
    account.amount = account.amount - price
    println(s"Bought - $thing")
    println("account balance " + account.toString)
  }

  for(_ <- 1 to 20){
    val account = new BankAccount(10000)
    val thread1 = new Thread(() => buy(account, "shoes", 300))
    val thread2 = new Thread(() => buy(account, "iphone", 500))

    thread1.start()
    thread2.start()
    Thread.sleep(100)
    println()

  }

  //Fixing race condition
  //1. use synchronized -> property of reference type -> no two threads can evaluate the reference at same time
  def buySafe(account: BankAccount, thing: String, price: Int) = {
    account.synchronized {
      account.amount -= price
      println(s"Bought - $thing")
      println("account balance " + account.toString)
    }
  }

  // use @volatile on a variable -> all reads and write to that variable are synchronized

  def inceptionThread(maxThread: Int, index: Int = 1): Thread = new Thread(() => {
    if (index < maxThread){
      val newThread = inceptionThread(maxThread, index + 1)
      newThread.start()
      newThread.join()
    }
    println("I am thread " + index)
  }
  )
}
