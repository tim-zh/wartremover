package org.brianmckenna.wartremover
package warts

object ListOps extends SimpleWartTraverser {

  class Op(name: String, error: String) extends SimpleWartTraverser {
    override lazy val className = "org.brianmckenna.wartremover.warts.ListOps"

    def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
      import u.universe._

      val listSymbol = rootMirror.staticClass("scala.collection.immutable.List")
      val Name: TermName = name
      tree match {
        case Select(left, Name) if left.tpe.baseType(listSymbol) != NoType ⇒
          err(u)(tree.pos, error)
        // TODO: This ignores a lot
        case LabelDef(_, _, rhs) if isSynthetic(u)(tree) ⇒ skip(u)
        case _ ⇒ continue(u)
      }
    }
  }

  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] =
    List[SimpleWartTraverser](
      new Op("head", "List#head is disabled - use List#headOption instead"),
      new Op("tail", "List#tail is disabled - use List#drop(1) instead"),
      new Op("init", "List#init is disabled - use List#dropRight(1) instead"),
      new Op("last", "List#last is disabled - use List#lastOption instead"),
      new Op("reduce", "List#reduce is disabled - use List#reduceOption or List#fold instead"),
      new Op("reduceLeft", "List#reduceLeft is disabled - use List#reduceLeftOption or List#foldLeft instead"),
      new Op("reduceRight", "List#reduceRight is disabled - use List#reduceRightOption or List#foldRight instead")
    ).reduce(_ ncompose _).traverse(u)(tree)

}
