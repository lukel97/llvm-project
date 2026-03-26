//===-- VPlanDominatorTree.h ------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements dominator tree analysis for a single level of a VPlan's
/// H-CFG.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_VECTORIZE_VPLANDOMINATORTREE_H
#define LLVM_TRANSFORMS_VECTORIZE_VPLANDOMINATORTREE_H

#include "VPlan.h"
#include "VPlanCFG.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Support/GenericDomTree.h"
#include "llvm/Support/GenericDomTreeConstruction.h"

namespace llvm {

template <> struct DomTreeNodeTraits<VPBlockBase> {
  using NodeType = VPBlockBase;
  using NodePtr = VPBlockBase *;
  using ParentPtr = VPlan *;

  static NodePtr getEntryNode(ParentPtr Parent) { return Parent->getEntry(); }
  static ParentPtr getParent(NodePtr B) { return B->getPlan(); }
};

/// Template specialization of the standard LLVM dominator tree utility for
/// VPBlockBases.
class VPDominatorTree : public DominatorTreeBase<VPBlockBase, false> {
  using Base = DominatorTreeBase<VPBlockBase, false>;

public:
  explicit VPDominatorTree(VPlan &Plan) { recalculate(Plan); }

  /// Returns true if \p A properly dominates \p B.
  using Base::properlyDominates;
  bool properlyDominates(const VPRecipeBase *A, const VPRecipeBase *B);
};

using VPDomTreeNode = DomTreeNodeBase<VPBlockBase>;

/// Template specializations of GraphTraits for VPDomTreeNode.
template <>
struct GraphTraits<VPDomTreeNode *>
    : public DomTreeGraphTraitsBase<VPDomTreeNode,
                                    VPDomTreeNode::const_iterator> {};

template <>
struct GraphTraits<const VPDomTreeNode *>
    : public DomTreeGraphTraitsBase<const VPDomTreeNode,
                                    VPDomTreeNode::const_iterator> {};

/// A lightweight wrapper representing a subgraph of a VPlan rooted at a
/// chosen entry block. Used as the parent type for VPPartialDominatorTree so
/// that the entry can be any VPBlockBase, not necessarily the VPlan entry.
struct VPBlockSubgraph {
  VPBlockBase *Entry;
};

/// GraphTraits for VPBlockSubgraph* – traversal starts at Entry using the
/// same successor-iterator as VPBlockBase*.
template <> struct GraphTraits<VPBlockSubgraph *> {
  using GraphRef = VPBlockSubgraph *;
  using NodeRef = VPBlockBase *;
  using nodes_iterator = df_iterator<NodeRef>;
  using ChildIteratorType = VPAllSuccessorsIterator<VPBlockBase *>;

  static NodeRef getEntryNode(GraphRef SG) { return SG->Entry; }

  static ChildIteratorType child_begin(NodeRef N) {
    return ChildIteratorType(N);
  }
  static ChildIteratorType child_end(NodeRef N) {
    return ChildIteratorType::end(N);
  }

  static nodes_iterator nodes_begin(GraphRef SG) {
    return nodes_iterator::begin(SG->Entry);
  }
  static nodes_iterator nodes_end(GraphRef SG) {
    return nodes_iterator::end(SG->Entry);
  }
};

/// DomTreeNodeTraits for a partial VPlan dominator tree. The parent is a
/// VPBlockSubgraph* carrying the chosen entry block. getParent() returns
/// nullptr because there is no single VPBlockBase field that stores which
/// subgraph a block belongs to; the assertions in DominatorTreeBase are
/// relaxed to treat a null return as "don't check".
struct VPPartialDomTreeTraits {
  using NodeType = VPBlockBase;
  using NodePtr = VPBlockBase *;
  using ParentPtr = VPBlockSubgraph *;

  static NodePtr getEntryNode(ParentPtr SG) { return SG->Entry; }
  /// Returns nullptr to opt out of the parent-equality assertion in
  /// DominatorTreeBase; the tree is still correct because construction only
  /// visits nodes reachable from the chosen entry.
  static ParentPtr getParent(NodePtr) { return nullptr; }
};

/// A dominator tree for a subgraph of a VPlan whose entry can be any
/// VPBlockBase, not necessarily the VPlan's own entry block.
class VPPartialDominatorTree
    : public DominatorTreeBase<VPBlockBase, false, VPPartialDomTreeTraits> {
  using Base = DominatorTreeBase<VPBlockBase, false, VPPartialDomTreeTraits>;
  VPBlockSubgraph SubgraphInfo;

public:
  explicit VPPartialDominatorTree(VPBlockBase *Entry) : SubgraphInfo{Entry} {
    recalculate(SubgraphInfo);
  }
};

} // namespace llvm
#endif // LLVM_TRANSFORMS_VECTORIZE_VPLANDOMINATORTREE_H
