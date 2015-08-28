
abstract trait MyTree[-A]{
  def isEmpty: Boolean
  def contains(it: A) : Boolean
  def add(it: A): MyTree[A]
}

case class Node[A](v: A, left: MyTree[A], right: MyTree[A])(implicit val ordering: Ordering[A]) extends MyTree[A]{
  override def contains(it: A): Boolean = {
    it == v || (if(ordering.compare(it, v) > 0)right else left).contains(it)
  }
  override def isEmpty: Boolean = false

  override def add(it: A)= {
    if(it == v) this
    else if(ordering.compare(it, v) > 0)
      if(right.isEmpty) Node(v, left, Node(it, Empty, Empty)) else Node(v, left, right.add(it))
    else
     if(left.isEmpty) Node(v, Node(it, Empty, Empty), right) else Node(v, left.add(it), right)

  }
}

case object Empty extends MyTree[Any]{
  override def isEmpty = true
  override def contains(it: Any): Boolean = false
  override def add(it: Any) = this
}


object Main {
  def main(args: Array[String]): Unit = {
    var bst : MyTree[Int] = Node(10, Empty, Empty)
    bst = bst.add(13).add(11).add(5).add(2).add(9).add(20)
    println(bst)
  }
}



