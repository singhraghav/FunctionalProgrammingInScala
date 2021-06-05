package parralelism

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
  smartProducerConsumer()
}
