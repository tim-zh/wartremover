package org.brianmckenna.wartremover
package warts

object Option2Iterable extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._

    val scala = newTypeName("scala")
    val option = newTermName("Option")
    val option2Iterable = newTermName("option2Iterable")

    tree match {
      case Select(Select(This(pkg), obj), method)
        if pkg == scala && obj == option && method == option2Iterable =>
        err(u)(tree.pos, "Implicit conversion from Option to Iterable is disabled - use Option#toIterable instead")
      case _ => continue(u)
    }
  }
}
