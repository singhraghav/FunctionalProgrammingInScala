package typeSystem

import scala.language.reflectiveCalls

object StructuralTypes extends App{

  type JavaClosable = java.io.Closeable

  class HipsterClosable {
    def close(): Unit = println("Hipster closable")
  }

//  def closeQuitely(closable: JavaClosable OR HipsterClosable)

  type UnifiedClosable = {
  def close(): Unit
  } // structural type

  def closeQuitely(unifiedClosable: UnifiedClosable): Unit = unifiedClosable.close()

  closeQuitely(new JavaClosable{
    override def close(): Unit = println("Java Closable")
  })

  closeQuitely(new HipsterClosable)

  //TYpe Refinement
  // java closable has been refined
  type AdvancedClosable = JavaClosable {
   def closeSilently(): Unit
  }

  class AdvancedjavaClosable extends JavaClosable {
    override def close(): Unit = println("advance java close")
    def closeSilently(): Unit = println("close silenelty")
  }

  def closeShh(advClosable: AdvancedClosable) = advClosable.closeSilently()
  closeShh(new AdvancedjavaClosable)
}
