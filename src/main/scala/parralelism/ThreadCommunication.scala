package parralelism

import scala.collection.mutable
import scala.util.Random

object ThreadCommunication extends App {
  /*
  the producer - consumer
  producer -> [ X ] -> consumer
  producer, consumer runs in parallel
  consumer should wait till the producer produces the data
  * */

  class SimpleContainer{
    private var value = 0
    def isEmpty: Boolean = value == 0
    def get = {
    val result =  value
      value = 0
      result
    }
    def set(v: Int) = value = v
  }

  def naiveProducerConsumer(): Unit = {
    val container = new SimpleContainer
    val consumer = new Thread(() =>{
      println("[consumer] waiting")
      // While loop wastes computing power because it uses resources
      while (container.isEmpty){
        println("[consumer] actively waiting")
      }
      println(s"[consumer] I have consumed ${container.get}")
    })

    val producer = new Thread(() => {
      println("[producer] computing ....")
      Thread.sleep(500)
      val value = 42
      println(s"[producer] I have produced $value")
      container.set(value)
    })

    consumer.start()
    producer.start()
  }

//  naiveProducerConsumer()

  def smartProducerConsumer() = {
    val container = new SimpleContainer
    val consumer = new Thread(() =>{
      println("[consumer] waiting")
      container.synchronized{
        container.wait() //puts the consumer to sleep
      }
      println(s"[consumer] I have consumed ${container.get}")
    })

    val producer = new Thread(() => {
      println("[producer] computing ....")
      Thread.sleep(500)
      val value = 42
      container.synchronized{
        container.set(value)
        container.notify() // wakes up the consumer after producing the value
      }
      println(s"[producer] I have produced $value")
    })

    consumer.start()
    producer.start()
  }
//  smartProducerConsumer()

  /*
  producer fills up the buffer and consumer consumes from buffer
  * producer -> [ ? ? ? ? ? ? ?] -> consumer
  once the buffer is full producer must stop
  once the buffer is empty the consumer must stop
  *
  * */

  class BufferContainer{
    var buffer = List.empty[Int]
    val maxSize = 10
    def isEmpty = buffer.isEmpty
    def isFull = buffer.size == maxSize
  }

  def prodConsLargeBuffer = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3
    val consumer = new Thread(() => {
      val random = new Random()
      while (true)
        {
          buffer.synchronized {
            if(buffer.isEmpty){
              println("[consumer] buffer empty, waiting ....")
              buffer.wait()
            }
            // atleast one value is in buffer
            val x = buffer.dequeue()
            println(s"[consumer] consumed $x" )
            buffer.notify()
          }

          Thread.sleep(random.nextInt(500))
        }
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0
      while (true)
      {
        buffer.synchronized {
          if(buffer.size == capacity){
            println("[producer] buffer full, waiting ....")
            buffer.wait()
          }
          // atleast one empty space in buffer
          println(s"[producer] produced $i")
          buffer.enqueue(i)
          i += 1
          buffer.notify()
        }
        Thread.sleep(random.nextInt(250))
      }
    })
    producer.start()
    consumer.start()
  }
//  prodConsLargeBuffer

  /*
  * limited capacity buffer with multiple producer and multiple consumer acting on same buffer
  * producer1 -> [ ? ? ? ? ] -> consumer1
  * producer2 -> ^         ^ -> consumer2
  * producer3 -> ^         ^ -> consumer3
  * */

  class Consumer(id: Int, buffer: mutable.Queue[Int]) extends Thread {
    override def run(): Unit = {
      val random = new Random()
      while (true)
      {
        buffer.synchronized {
          while (buffer.isEmpty){
            println(s"[consumer $id] buffer empty, waiting ....")
            buffer.wait()
          }
          // atleast one value is in buffer
          val x = buffer.dequeue()
          println(s"[consumer $id] consumed $x" )
          buffer.notify()
        }

        Thread.sleep(random.nextInt(500))
      }
    }
  }

  class Producer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread {
    override def run(): Unit = {
      val random = new Random()
      var i = 0
      while (true)
      {
        buffer.synchronized {
          while(buffer.size == capacity){
            println(s"[producer $id] buffer full, waiting ....")
            buffer.wait()
          }
          // atleast one empty space in buffer
          println(s"[producer $id] produced $i")
          buffer.enqueue(i)
          i += 1
          buffer.notify()
        }
        Thread.sleep(random.nextInt(500))
      }
    }
  }

  /*
  * notify can wake both producer and consumer and since we have multiple of both
  * notify in consumer may awake the consumer so have an only if check will fail and same for producer
  * */
  def multipleProdConsLargeBuffer = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3
    val producer1 = new Producer(1, buffer, capacity)
    val producer2 = new Producer(2, buffer, capacity)
    val producer3 = new Producer(3, buffer, capacity)

    val consumer1 = new Consumer(1, buffer)
    val consumer2 = new Consumer(2, buffer)
    val consumer3 = new Consumer(3, buffer)

    producer1.start()
    producer2.start()
    producer3.start()

    consumer1.start()
    consumer2.start()
    consumer3.start()
  }

  multipleProdConsLargeBuffer
}


















