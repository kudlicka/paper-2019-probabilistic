class BiSSENode {
  taxon:Integer?;
  branch_length:Real;
  t_beg:Real;
  t_end:Real;
  left:BiSSENode?;
  right:BiSSENode?;
  parent:BiSSENode&;
  state_end:Integer?;

  function isRoot() -> Boolean {
    parent:BiSSENode? <- this.parent;
    return !parent?;
  }

  function getParent() -> BiSSENode? {
    return parent;
  }

  function hasChildren() -> Boolean {
    return left? || right?;
  }

  function noStalk() -> Boolean {
    return branch_length < 1e-5;
  }

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

  function length() -> Real {
    length:Real <- branch_length;
    if left? {
      length <- length + left!.length();
    }
    if right? {
      length <- length + right!.length();
    }
    return length;
  }
}
