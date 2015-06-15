package org.brianmckenna.wartremover
package warts

object Any2StringAdd extends SimpleWartTraverser {
  def traverse(u: WartUniverse)(tree: u.Tree): List[Traversal { type Universe = u.type }] = {
    import u.universe._

    val PredefName: TermName = "Predef"
    val Any2StringAddName: TermName = "any2stringadd"

    tree match {
      case Apply(Select(Select(_, PredefName), Any2StringAddName), _) =>
        err(u)(tree.pos, "Scala inserted an any2stringadd call")
      case TypeApply(Select(Select(_, PredefName), Any2StringAddName), _) =>
        err(u)(tree.pos, "Scala inserted an any2stringadd call")
      case _ => continue(u)
    }
  }
}
