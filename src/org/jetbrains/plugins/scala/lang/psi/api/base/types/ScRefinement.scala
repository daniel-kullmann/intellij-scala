package org.jetbrains.plugins.scala.lang.psi.api.base.types

import statements.{ScDeclaredElementsHolder, ScTypeAlias}
import psi.ScalaPsiElement
/**
* @author Alexander Podkhalyuzin
* Date: 07.03.2008
*/

trait ScRefinement extends ScalaPsiElement {
  def holders() : Seq[ScDeclaredElementsHolder] = Seq(findChildrenByClass(classOf[ScDeclaredElementsHolder]): _*)
  def types() : Seq[ScTypeAlias] = Seq(findChildrenByClass(classOf[ScTypeAlias]): _*)
}