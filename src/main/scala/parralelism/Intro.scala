package parralelism

import java.util.concurrent.Executors

object Intro extends App {
  // JVM threads
  val aThread = new Thread(() => {
    println("Running in parralel")
  })

  aThread.start()
  // create a jvm thread which run on top of os thread
  // the thread is a different one than on which main method is executed

  aThread.join() // block until AThread finishes running

  val threadHello = new Thread(() => {1 to 5 foreach(_ => println("hello"))})
  val threadBye = new Thread(() => {1 to 5 foreach(_ => println("Good Bye"))})
  threadHello.start()
  threadBye.start()

  // thread runs in random order

  // executors -> Thread is very costly
  //so we create a thread pool instead of killing we reuse

  val pool = Executors.newFixedThreadPool(10)
  pool.execute(() => {println("Something in thread pool")})

  pool.execute(() => {Thread.sleep(1000) ; println("Done after 1 second")})
  pool.execute(() => {Thread.sleep(1000) ; println("Almost done second")})

  pool.shutdown() // shutdown all thread now no thread from pool will execute anything -> previously submitted task are executed
  pool.shutdownNow() // it even stops the sleeping thread -> even previously submitted task throws exception
}
