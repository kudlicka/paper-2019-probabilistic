class PhyNode {
  taxon:Integer?;
  branch_length:Real;
  t_beg:Real;
  t_end:Real;
  left:PhyNode?;
  right:PhyNode?;
  parent:PhyNode&;

  function isRoot() -> Boolean {
    parent:PhyNode? <- this.parent;
    return !parent?;
  }

  function hasChildren() -> Boolean {
    return left? || right?;
  }

  function noStalk() -> Boolean {
    return branch_length < 1e-5;
  }

  // Return count of the nodes in the clade with the current node being its root
  function count() -> Integer {
    count:Integer <- 1;
    if left? {
      count <- count + left!.count();
    }
    if right? {
      count <- count + right!.count();
    }
    return count;
  }

  function printClade(prefix:String) {
    stdout.print(prefix + t_beg + " --> " + t_end + "\n");
    if left? {
      left!.printClade(prefix + "  ");
    }
    if right? {
      right!.printClade(prefix + "  ");
    }
  }
}
