package org.jetbrains.plugins.scala.lang
package resolve
package processor
package precedence

import scala.collection.mutable

trait TopPrecedence[Repr] {

  /**
    * Returns highest precedence of all resolve results.
    * 1 - import a._
    * 2 - import a.x
    * 3 - definition or declaration
    */
  def apply(result: ScalaResolveResult): Int

  def update(result: ScalaResolveResult, i: Int): Unit

  def filterNot(precedence: Int,
                left: ScalaResolveResult,
                right: ScalaResolveResult): Boolean =
    precedence < apply(right)

  implicit def toRepresentation(result: ScalaResolveResult): Repr
}

abstract class TopPrecedenceImpl[Repr] extends TopPrecedence[Repr] {

  private val precedence = new mutable.HashMap[Repr, Int]()

  override def apply(result: ScalaResolveResult): Int =
    precedence.getOrElse(result, 0)

  override def update(result: ScalaResolveResult, i: Int): Unit = {
    precedence.put(result, i)
  }

  override def filterNot(precedence: Int,
                         left: ScalaResolveResult,
                         right: ScalaResolveResult): Boolean =
    toRepresentation(left) == toRepresentation(right) &&
      super.filterNot(precedence, left, right)
}

object TopPrecedenceImpl {

  private[processor] implicit def toStringRepresentation(result: ScalaResolveResult): String =
    result.isRenamed.getOrElse(result.name)
}
