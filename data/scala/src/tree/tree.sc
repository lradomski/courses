import tree.Node

val r = new Node(10)
val ll = r.lleaf(5)
val rl = r.rleaf(8)
//r
val l = r.toSortedList
r.inorder()
r.preorder()
r.postorder()

l == r.inorder()
l == r.postorder()