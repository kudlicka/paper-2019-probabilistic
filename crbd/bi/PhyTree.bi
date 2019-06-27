class PhyTree<T> {
  root:T?;

  function setRoot(node:T) {
    root <- node;
  }

  function count() -> Integer {
    if root? {
      return root!.count();
    }
    return 0;
  }

  fiber depthFirstTraversal() -> T {
    node:T? <- root;

    while node? {
      yield node!;

      if node!.left? {
        node <- node!.left!;
      } else if node!.right? {
        node <- node!.right!;
      } else {
        parent:T? <- node!.parent;
        while parent? {
          if parent!.left? && parent!.left! == node! && parent!.right? { 
            node <- parent!.right;
            parent <- nil;
          } else {
            node <- parent!;
            parent <- node!.parent;
          }
        }
        if root! == node! {
          return;
        }
      }
    }
  }

  function print() {
    if root? {
      root!.printClade("");
    }
  }

  function loadNode(node:T, reader:Reader) {
    node.taxon <- reader.getInteger("taxon");
    node.branch_length <- reader.getReal("branch_length")!;
    node.t_end <- 0.0;
    node.t_beg <- node.branch_length;

    childReader:Reader! <- reader.getArray("children");
    while (childReader?) {
      child:T;
      child.parent <- node;
      loadNode(child, childReader!);
      if !node.left? {
        node.left <- child;
      } else if !node.right? {
        node.right <- child;
      } else {
        assert false;
      }
      node.t_end <- child.t_beg;
      node.t_beg <- node.t_end + node.branch_length;
    }
  }

  function load(reader:Reader) {
    node:T;
    tree:Reader! <- reader.getArray("trees");
    if tree? {
      // We will load the first tree in the file
      loadNode(node, tree!.getObject("root")!);
    }
    setRoot(node);
  }
}

class PlainPhyTree < PhyTree<PhyNode> {
}
