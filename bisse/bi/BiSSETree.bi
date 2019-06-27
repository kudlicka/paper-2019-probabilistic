class BiSSETree {
  root:BiSSENode?;

  function setRoot(node:BiSSENode) {
    root <- node;
  }

  function count() -> Integer {
    if root? {
      return root!.count();
    }
    return 0;
  }

  function totalLength() -> Real {
    if root? {
      return root!.length();
    }
    return 0;
  }

  fiber depthFirstTraversal() -> BiSSENode {
    node:BiSSENode? <- root;

    while node? {
      yield node!;

      if node!.left? {
        node <- node!.left!;
      } else if node!.right? {
        node <- node!.right!;
      } else {
        parent:BiSSENode? <- node!.parent;
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

  function loadNode(node:BiSSENode, reader:Reader, states:Integer?[_]) {
    node.taxon <- reader.getInteger("taxon");
    node.branch_length <- reader.getReal("branch_length")!;
    node.t_end <- 0.0;
    node.t_beg <- node.branch_length;
    if node.taxon? && length(states) >= 1+node.taxon! {
      node.state_end <- states[1+node.taxon!];
    }

    childReader:Reader! <- reader.getArray("children");
    while (childReader?) {
      child:BiSSENode;
      child.parent <- node;
      loadNode(child, childReader!, states);
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
    node:BiSSENode;
    states:Integer?[_];
    taxa:Reader! <- reader.getArray("taxa");
    while taxa? {
      characters:Reader! <- reader.getArray("characters");
      k:Integer <- 1 + (taxa!.getInteger("id")!);
      v:Integer?;
      if taxa!.getString(["characters", "state"])? {
        if taxa!.getString(["characters", "state"])! == "1" {
          v <- 1;
        } else if (taxa!.getString(["characters", "state"])! == "0") {
          v <- 0;
        }
      }
      default:Integer? <- nil;
      cpp {{
      states_.enlarge(bi::make_frame(k_), default_);
      }}
      states[k] <- v;
    }

    tree:Reader! <- reader.getArray("trees");
    if tree? {
      // We will load the first tree in the file
      loadNode(node, tree!.getObject("root")!, states);
    }
    setRoot(node);
  }
}
