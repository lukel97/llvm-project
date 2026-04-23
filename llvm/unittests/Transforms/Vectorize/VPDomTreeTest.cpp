//===- llvm/unittests/Transforms/Vectorize/VPDomTreeTests.cpp - -----------===//
//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "../lib/Transforms/Vectorize/VPlan.h"
#include "../lib/Transforms/Vectorize/VPlanDominatorTree.h"
#include "VPlanTestBase.h"
#include "gtest/gtest.h"

namespace llvm {
namespace {

using VPDominatorTreeTest = VPlanTestBase;

TEST_F(VPDominatorTreeTest, DominanceNoRegionsTest) {
  //   VPBB0
  //    |
  //   R1 {
  //     VPBB1
  //     /   \
    // VPBB2  VPBB3
  //    \    /
  //    VPBB4
  //  }
  VPlan &Plan = getPlan();
  VPBasicBlock *VPBB0 = Plan.getEntry();
  VPBasicBlock *VPBB1 = Plan.createVPBasicBlock("VPBB1");
  VPBasicBlock *VPBB2 = Plan.createVPBasicBlock("VPBB2");
  VPBasicBlock *VPBB3 = Plan.createVPBasicBlock("VPBB3");
  VPBasicBlock *VPBB4 = Plan.createVPBasicBlock("VPBB4");
  VPRegionBlock *R1 = Plan.createLoopRegion("R1", VPBB1, VPBB4);
  VPBB2->setParent(R1);
  VPBB3->setParent(R1);

  VPBlockUtils::connectBlocks(VPBB0, R1);
  VPBlockUtils::connectBlocks(VPBB1, VPBB2);
  VPBlockUtils::connectBlocks(VPBB1, VPBB3);
  VPBlockUtils::connectBlocks(VPBB2, VPBB4);
  VPBlockUtils::connectBlocks(VPBB3, VPBB4);

  VPBlockUtils::connectBlocks(R1, Plan.getScalarHeader());

  VPDominatorTree VPDT(Plan);

  EXPECT_TRUE(VPDT.dominates(VPBB1, VPBB4));
  EXPECT_FALSE(VPDT.dominates(VPBB4, VPBB1));

  EXPECT_TRUE(VPDT.dominates(VPBB1, VPBB2));
  EXPECT_FALSE(VPDT.dominates(VPBB2, VPBB1));

  EXPECT_TRUE(VPDT.dominates(VPBB1, VPBB3));
  EXPECT_FALSE(VPDT.dominates(VPBB3, VPBB1));

  EXPECT_EQ(VPDT.findNearestCommonDominator(VPBB2, VPBB3), VPBB1);
  EXPECT_EQ(VPDT.findNearestCommonDominator(VPBB2, VPBB4), VPBB1);
  EXPECT_EQ(VPDT.findNearestCommonDominator(VPBB4, VPBB4), VPBB4);
}

static void
checkDomChildren(VPDominatorTree &VPDT, VPBlockBase *Src,
                 std::initializer_list<VPBlockBase *> ExpectedChildren) {
  SmallVector<VPDomTreeNode *> Children(VPDT.getNode(Src)->children());
  SmallVector<VPDomTreeNode *> ExpectedNodes;
  for (VPBlockBase *C : ExpectedChildren)
    ExpectedNodes.push_back(VPDT.getNode(C));

  EXPECT_EQ(Children, ExpectedNodes);
}

TEST_F(VPDominatorTreeTest, DominanceRegionsTest) {
  {
    // 2 consecutive regions.
    // VPBB0
    //  |
    // R1 {
    //     \
    //     R1BB1     _
    //    /     \   / \
    //  R1BB2   R1BB3  |
    //    \      /  \_/
    //     R1BB4
    //  }
    //   |
    // R2 {
    //   \
    //    R2BB1
    //      |
    //    R2BB2
    // }
    //
    VPlan &Plan = getPlan();
    VPBasicBlock *VPBB0 = Plan.getEntry();
    VPBasicBlock *R1BB1 = Plan.createVPBasicBlock("");
    VPBasicBlock *R1BB2 = Plan.createVPBasicBlock("");
    VPBasicBlock *R1BB3 = Plan.createVPBasicBlock("");
    VPBasicBlock *R1BB4 = Plan.createVPBasicBlock("");
    VPRegionBlock *R1 = Plan.createLoopRegion("R1", R1BB1, R1BB4);
    R1BB2->setParent(R1);
    R1BB3->setParent(R1);
    VPBlockUtils::connectBlocks(VPBB0, R1);
    VPBlockUtils::connectBlocks(R1BB1, R1BB2);
    VPBlockUtils::connectBlocks(R1BB1, R1BB3);
    VPBlockUtils::connectBlocks(R1BB2, R1BB4);
    VPBlockUtils::connectBlocks(R1BB3, R1BB4);
    // Cycle.
    VPBlockUtils::connectBlocks(R1BB3, R1BB3);

    VPBasicBlock *R2BB1 = Plan.createVPBasicBlock("");
    VPBasicBlock *R2BB2 = Plan.createVPBasicBlock("");
    VPRegionBlock *R2 = Plan.createLoopRegion("R2", R2BB1, R2BB2);
    VPBlockUtils::connectBlocks(R2BB1, R2BB2);
    VPBlockUtils::connectBlocks(R1, R2);

    VPBlockUtils::connectBlocks(R2, Plan.getScalarHeader());
    VPDominatorTree VPDT(Plan);

    checkDomChildren(VPDT, R1, {R1BB1});
    checkDomChildren(VPDT, R1BB1, {R1BB2, R1BB4, R1BB3});
    checkDomChildren(VPDT, R1BB2, {});
    checkDomChildren(VPDT, R1BB3, {});
    checkDomChildren(VPDT, R1BB4, {R2});
    checkDomChildren(VPDT, R2, {R2BB1});
    checkDomChildren(VPDT, R2BB1, {R2BB2});

    EXPECT_TRUE(VPDT.dominates(R1, R2));
    EXPECT_FALSE(VPDT.dominates(R2, R1));

    EXPECT_TRUE(VPDT.dominates(R1BB1, R1BB4));
    EXPECT_FALSE(VPDT.dominates(R1BB4, R1BB1));

    EXPECT_TRUE(VPDT.dominates(R2BB1, R2BB2));
    EXPECT_FALSE(VPDT.dominates(R2BB2, R2BB1));

    EXPECT_TRUE(VPDT.dominates(R1BB1, R2BB1));
    EXPECT_FALSE(VPDT.dominates(R2BB1, R1BB1));

    EXPECT_TRUE(VPDT.dominates(R1BB4, R2BB1));
    EXPECT_FALSE(VPDT.dominates(R1BB3, R2BB1));

    EXPECT_TRUE(VPDT.dominates(R1, R2BB1));
    EXPECT_FALSE(VPDT.dominates(R2BB1, R1));
  }

  {
    // 2 nested regions.
    //  VPBB1
    //    |
    //  R1 {
    //         R1BB1
    //       /        \
    //   R2 {          |
    //     \           |
    //     R2BB1       |
    //       |   \    R1BB2
    //     R2BB2-/     |
    //        \        |
    //         R2BB3   |
    //   }            /
    //      \        /
    //        R1BB3
    //  }
    //   |
    //  VPBB2
    //
    VPlan &Plan = getPlan();
    VPBasicBlock *R1BB1 = Plan.createVPBasicBlock("R1BB1");
    VPBasicBlock *R1BB2 = Plan.createVPBasicBlock("R1BB2");
    VPBasicBlock *R1BB3 = Plan.createVPBasicBlock("R1BB3");
    VPRegionBlock *R1 = Plan.createLoopRegion("R1", R1BB1, R1BB3);

    VPBasicBlock *R2BB1 = Plan.createVPBasicBlock("R2BB1");
    VPBasicBlock *R2BB2 = Plan.createVPBasicBlock("R2BB2");
    VPBasicBlock *R2BB3 = Plan.createVPBasicBlock("R2BB3");
    VPRegionBlock *R2 = Plan.createLoopRegion("R2", R2BB1, R2BB3);
    R2BB2->setParent(R2);
    VPBlockUtils::connectBlocks(R2BB1, R2BB2);
    VPBlockUtils::connectBlocks(R2BB2, R2BB1);
    VPBlockUtils::connectBlocks(R2BB2, R2BB3);

    R2->setParent(R1);
    VPBlockUtils::connectBlocks(R1BB1, R2);
    R1BB2->setParent(R1);
    VPBlockUtils::connectBlocks(R1BB1, R1BB2);
    VPBlockUtils::connectBlocks(R1BB2, R1BB3);
    VPBlockUtils::connectBlocks(R2, R1BB3);

    VPBasicBlock *VPBB1 = Plan.getEntry();
    VPBlockUtils::connectBlocks(VPBB1, R1);
    VPBasicBlock *VPBB2 = Plan.createVPBasicBlock("VPBB2");
    VPBlockUtils::connectBlocks(R1, VPBB2);

    VPBlockUtils::connectBlocks(VPBB2, Plan.getScalarHeader());
    VPDominatorTree VPDT(Plan);

    checkDomChildren(VPDT, VPBB1, {R1});
    checkDomChildren(VPDT, R1, {R1BB1});
    checkDomChildren(VPDT, R1BB1, {R2, R1BB3, R1BB2});
    checkDomChildren(VPDT, R1BB2, {});
    checkDomChildren(VPDT, R2, {R2BB1});
    checkDomChildren(VPDT, R2BB1, {R2BB2});
    checkDomChildren(VPDT, R2BB2, {R2BB3});
    checkDomChildren(VPDT, R2BB3, {});
    checkDomChildren(VPDT, R1BB3, {VPBB2});
    checkDomChildren(VPDT, VPBB2, {Plan.getScalarHeader()});
  }

  {
    // 2 nested replicate regions.
    //  VPBB1
    //    |
    //  R1 {
    //         R1BB1
    //       /        \
    //   R2 {          |
    //     \           |
    //     R2BB1       |
    //       |   \    R1BB2
    //     R2BB2-/     |
    //        \        |
    //         R2BB3   |
    //   }            /
    //      \        /
    //        R1BB3
    //  }
    //   |
    //  VPBB2
    //
    VPlan &Plan = getPlan();
    VPBasicBlock *R1BB1 = Plan.createVPBasicBlock("R1BB1");
    VPInstruction *R1BB1I = new VPInstruction(VPInstruction::VScale, {});
    R1BB1->appendRecipe(R1BB1I);
    VPBasicBlock *R1BB2 = Plan.createVPBasicBlock("R1BB2");
    VPInstruction *R1BB2I = new VPInstruction(VPInstruction::VScale, {});
    R1BB2->appendRecipe(R1BB2I);
    VPBasicBlock *R1BB3 = Plan.createVPBasicBlock("R1BB3");
    VPInstruction *R1BB3I = new VPInstruction(VPInstruction::VScale, {});
    R1BB3->appendRecipe(R1BB3I);
    VPRegionBlock *R1 = Plan.createReplicateRegion(R1BB1, R1BB3, "R1");

    VPBasicBlock *R2BB1 = Plan.createVPBasicBlock("R2BB1");
    VPInstruction *R2BB1I = new VPInstruction(VPInstruction::VScale, {});
    R2BB1->appendRecipe(R2BB1I);
    VPBasicBlock *R2BB2 = Plan.createVPBasicBlock("R2BB2");
    VPInstruction *R2BB2I = new VPInstruction(VPInstruction::VScale, {});
    R2BB2->appendRecipe(R2BB2I);
    VPBasicBlock *R2BB3 = Plan.createVPBasicBlock("R2BB3");
    VPInstruction *R2BB3I = new VPInstruction(VPInstruction::VScale, {});
    R2BB3->appendRecipe(R2BB3I);
    VPRegionBlock *R2 = Plan.createReplicateRegion(R2BB1, R2BB3, "R2");
    R2BB2->setParent(R2);
    VPBlockUtils::connectBlocks(R2BB1, R2BB2);
    VPBlockUtils::connectBlocks(R2BB2, R2BB1);
    VPBlockUtils::connectBlocks(R2BB2, R2BB3);

    R2->setParent(R1);
    VPBlockUtils::connectBlocks(R1BB1, R2);
    R1BB2->setParent(R1);
    VPBlockUtils::connectBlocks(R1BB1, R1BB2);
    VPBlockUtils::connectBlocks(R1BB2, R1BB3);
    VPBlockUtils::connectBlocks(R2, R1BB3);

    VPBasicBlock *VPBB1 = Plan.getEntry();
    VPBlockUtils::connectBlocks(VPBB1, R1);
    VPBasicBlock *VPBB2 = Plan.createVPBasicBlock("VPBB2");
    VPBlockUtils::connectBlocks(R1, VPBB2);

    VPBlockUtils::connectBlocks(VPBB2, Plan.getScalarHeader());
    VPDominatorTree VPDT(Plan);

    EXPECT_TRUE(VPDT.properlyDominates(R1BB1I, R2BB1I));
    EXPECT_TRUE(VPDT.properlyDominates(R1BB1I, R2BB2I));
    EXPECT_TRUE(VPDT.properlyDominates(R1BB1I, R2BB3I));
    EXPECT_TRUE(VPDT.properlyDominates(R1BB1I, R1BB3I));
    EXPECT_FALSE(VPDT.properlyDominates(R1BB2I, R1BB3I));
    EXPECT_TRUE(VPDT.properlyDominates(R2BB1I, R2BB2I));
    EXPECT_TRUE(VPDT.properlyDominates(R2BB1I, R2BB3I));
    EXPECT_FALSE(VPDT.properlyDominates(R2BB1I, R1BB3I));
    EXPECT_FALSE(VPDT.properlyDominates(R2BB3I, R2BB2I));
    EXPECT_FALSE(VPDT.properlyDominates(R2BB1I, R2BB1I));
  }
}

using VPPartialDominatorTreeTest = VPlanTestBase;

/// Helper matching checkDomChildren but for VPPartialDominatorTree.
static void
checkPartialDomChildren(VPPartialDominatorTree &VPDT, VPBlockBase *Src,
                        std::initializer_list<VPBlockBase *> ExpectedChildren) {
  SmallVector<VPDomTreeNode *> Children(VPDT.getNode(Src)->children());
  SmallVector<VPDomTreeNode *> ExpectedNodes;
  for (VPBlockBase *C : ExpectedChildren)
    ExpectedNodes.push_back(VPDT.getNode(C));
  EXPECT_EQ(Children, ExpectedNodes);
}

TEST_F(VPPartialDominatorTreeTest, PartialDominanceNoRegionsTest) {
  // VPBB0 -> VPBB1 -> VPBB2 -> VPBB4 -> ScalarHeader
  //              \-> VPBB3 ->/
  //
  // Build a partial dom tree rooted at VPBB1 (not the VPlan entry VPBB0).
  // VPBB0 must not appear in the tree; ScalarHeader is reachable so it does.
  VPlan &Plan = getPlan();
  VPBasicBlock *VPBB0 = Plan.getEntry();
  VPBasicBlock *VPBB1 = Plan.createVPBasicBlock("VPBB1");
  VPBasicBlock *VPBB2 = Plan.createVPBasicBlock("VPBB2");
  VPBasicBlock *VPBB3 = Plan.createVPBasicBlock("VPBB3");
  VPBasicBlock *VPBB4 = Plan.createVPBasicBlock("VPBB4");

  VPBlockUtils::connectBlocks(VPBB0, VPBB1);
  VPBlockUtils::connectBlocks(VPBB1, VPBB2);
  VPBlockUtils::connectBlocks(VPBB1, VPBB3);
  VPBlockUtils::connectBlocks(VPBB2, VPBB4);
  VPBlockUtils::connectBlocks(VPBB3, VPBB4);
  VPBlockUtils::connectBlocks(VPBB4, Plan.getScalarHeader());

  // Partial tree rooted at VPBB1 – does not include VPBB0.
  VPPartialDominatorTree VPDT(VPBB1);

  // VPBB0 was not reachable from the chosen entry; it has no node.
  EXPECT_EQ(VPDT.getNode(VPBB0), nullptr);

  // ScalarHeader is reachable from VPBB1 (via VPBB4) and is in the tree.
  EXPECT_NE(VPDT.getNode(Plan.getScalarHeader()), nullptr);

  // VPBB1 is the root and dominates everything reachable from it.
  EXPECT_TRUE(VPDT.dominates(VPBB1, VPBB1));
  EXPECT_TRUE(VPDT.dominates(VPBB1, VPBB2));
  EXPECT_TRUE(VPDT.dominates(VPBB1, VPBB3));
  EXPECT_TRUE(VPDT.dominates(VPBB1, VPBB4));
  EXPECT_TRUE(VPDT.dominates(VPBB1, Plan.getScalarHeader()));

  EXPECT_FALSE(VPDT.dominates(VPBB2, VPBB1));
  EXPECT_FALSE(VPDT.dominates(VPBB3, VPBB1));
  EXPECT_FALSE(VPDT.dominates(VPBB4, VPBB1));

  // Neither VPBB2 nor VPBB3 dominates the other.
  EXPECT_FALSE(VPDT.dominates(VPBB2, VPBB3));
  EXPECT_FALSE(VPDT.dominates(VPBB3, VPBB2));

  // VPBB4 dominates ScalarHeader.
  EXPECT_TRUE(VPDT.dominates(VPBB4, Plan.getScalarHeader()));

  // Nearest common dominator queries.
  EXPECT_EQ(VPDT.findNearestCommonDominator(VPBB2, VPBB3), VPBB1);
  EXPECT_EQ(VPDT.findNearestCommonDominator(VPBB2, VPBB4), VPBB1);
  EXPECT_EQ(VPDT.findNearestCommonDominator(VPBB4, VPBB4), VPBB4);
  EXPECT_EQ(VPDT.findNearestCommonDominator(VPBB1, VPBB4), VPBB1);

  // Verify dominator-tree children structure.
  // The diamond join point VPBB4 is an immediate dominator child of VPBB1.
  checkPartialDomChildren(VPDT, VPBB1, {VPBB2, VPBB4, VPBB3});
  checkPartialDomChildren(VPDT, VPBB2, {});
  checkPartialDomChildren(VPDT, VPBB3, {});
  checkPartialDomChildren(VPDT, VPBB4, {Plan.getScalarHeader()});
  checkPartialDomChildren(VPDT, Plan.getScalarHeader(), {});

  // Build a second partial tree rooted at VPBB3.
  // VPBB3 -> VPBB4 -> ScalarHeader; VPBB0, VPBB1, and VPBB2 are not reachable.
  VPPartialDominatorTree VPDT3(VPBB3);

  EXPECT_EQ(VPDT3.getNode(VPBB0), nullptr);
  EXPECT_EQ(VPDT3.getNode(VPBB1), nullptr);
  EXPECT_EQ(VPDT3.getNode(VPBB2), nullptr);

  EXPECT_TRUE(VPDT3.dominates(VPBB3, VPBB4));
  EXPECT_TRUE(VPDT3.dominates(VPBB3, Plan.getScalarHeader()));
  EXPECT_TRUE(VPDT3.dominates(VPBB4, Plan.getScalarHeader()));
  EXPECT_FALSE(VPDT3.dominates(VPBB4, VPBB3));
}

TEST_F(VPPartialDominatorTreeTest, PartialDominanceInsideRegionTest) {
  // VPBB0
  //  |
  // R1 {
  //    R1BB1       <- partial tree entry
  //   /     \
  // R1BB2  R1BB3
  //   \     /
  //   R1BB4
  // }
  //  |
  // VPBB5 -> ScalarHeader
  //
  // Build a partial dom tree rooted at R1BB1.
  // The region block R1 and VPBB0 must not appear in the tree.
  // VPAllSuccessorsIterator follows region exits, so VPBB5 and ScalarHeader
  // are reachable from R1BB4 and are included in the tree.
  VPlan &Plan = getPlan();
  VPBasicBlock *VPBB0 = Plan.getEntry();
  VPBasicBlock *R1BB1 = Plan.createVPBasicBlock("R1BB1");
  VPBasicBlock *R1BB2 = Plan.createVPBasicBlock("R1BB2");
  VPBasicBlock *R1BB3 = Plan.createVPBasicBlock("R1BB3");
  VPBasicBlock *R1BB4 = Plan.createVPBasicBlock("R1BB4");
  VPRegionBlock *R1 = Plan.createLoopRegion("R1", R1BB1, R1BB4);
  R1BB2->setParent(R1);
  R1BB3->setParent(R1);

  VPBlockUtils::connectBlocks(VPBB0, R1);
  VPBlockUtils::connectBlocks(R1BB1, R1BB2);
  VPBlockUtils::connectBlocks(R1BB1, R1BB3);
  VPBlockUtils::connectBlocks(R1BB2, R1BB4);
  VPBlockUtils::connectBlocks(R1BB3, R1BB4);

  VPBasicBlock *VPBB5 = Plan.createVPBasicBlock("VPBB5");
  VPBlockUtils::connectBlocks(R1, VPBB5);
  VPBlockUtils::connectBlocks(VPBB5, Plan.getScalarHeader());

  // Partial tree rooted at R1BB1.
  VPPartialDominatorTree VPDT(R1BB1);

  // VPBB0 and R1 were not reached from R1BB1; they have no nodes.
  EXPECT_EQ(VPDT.getNode(VPBB0), nullptr);
  EXPECT_EQ(VPDT.getNode(R1), nullptr);

  // VPAllSuccessorsIterator crosses region exits: VPBB5 is reachable from
  // R1BB4 via R1's successor list.
  EXPECT_NE(VPDT.getNode(VPBB5), nullptr);
  EXPECT_NE(VPDT.getNode(Plan.getScalarHeader()), nullptr);

  // R1BB1 is the root.
  EXPECT_TRUE(VPDT.dominates(R1BB1, R1BB1));
  EXPECT_TRUE(VPDT.dominates(R1BB1, R1BB2));
  EXPECT_TRUE(VPDT.dominates(R1BB1, R1BB3));
  EXPECT_TRUE(VPDT.dominates(R1BB1, R1BB4));
  EXPECT_TRUE(VPDT.dominates(R1BB1, VPBB5));
  EXPECT_TRUE(VPDT.dominates(R1BB1, Plan.getScalarHeader()));

  EXPECT_FALSE(VPDT.dominates(R1BB4, R1BB1));
  EXPECT_FALSE(VPDT.dominates(R1BB2, R1BB3));
  EXPECT_FALSE(VPDT.dominates(R1BB3, R1BB2));

  // R1BB4 dominates the post-region blocks.
  EXPECT_TRUE(VPDT.dominates(R1BB4, VPBB5));
  EXPECT_TRUE(VPDT.dominates(R1BB4, Plan.getScalarHeader()));

  EXPECT_EQ(VPDT.findNearestCommonDominator(R1BB2, R1BB3), R1BB1);
  EXPECT_EQ(VPDT.findNearestCommonDominator(R1BB2, R1BB4), R1BB1);

  checkPartialDomChildren(VPDT, R1BB1, {R1BB2, R1BB4, R1BB3});
  checkPartialDomChildren(VPDT, R1BB2, {});
  checkPartialDomChildren(VPDT, R1BB3, {});
  checkPartialDomChildren(VPDT, R1BB4, {VPBB5});
  checkPartialDomChildren(VPDT, VPBB5, {Plan.getScalarHeader()});
}

TEST_F(VPPartialDominatorTreeTest, PartialDominanceWithCycleTest) {
  // R1 {
  //   R1BB1        <- partial tree entry; loop latch cycles back
  //     |
  //   R1BB2
  //   / \
  // R1BB3  (backedge -> R1BB1)
  //
  // The sub-graph traversal follows VPAllSuccessorsIterator semantics, so
  // the backedge creates a cycle. The partial dominator tree should still
  // compute valid dominance.
  VPlan &Plan = getPlan();
  VPBasicBlock *VPBB0 = Plan.getEntry();
  VPBasicBlock *R1BB1 = Plan.createVPBasicBlock("R1BB1");
  VPBasicBlock *R1BB2 = Plan.createVPBasicBlock("R1BB2");
  VPBasicBlock *R1BB3 = Plan.createVPBasicBlock("R1BB3");
  VPRegionBlock *R1 = Plan.createLoopRegion("R1", R1BB1, R1BB3);
  R1BB2->setParent(R1);

  VPBlockUtils::connectBlocks(VPBB0, R1);
  VPBlockUtils::connectBlocks(R1BB1, R1BB2);
  VPBlockUtils::connectBlocks(R1BB2, R1BB3);
  // Backedge: the latch R1BB3 loops back to R1BB1.
  VPBlockUtils::connectBlocks(R1BB3, R1BB1);

  VPBlockUtils::connectBlocks(R1, Plan.getScalarHeader());

  VPPartialDominatorTree VPDT(R1BB1);

  // Blocks outside the subgraph have no node.
  EXPECT_EQ(VPDT.getNode(VPBB0), nullptr);
  EXPECT_EQ(VPDT.getNode(R1), nullptr);

  // R1BB1 dominates all blocks in the loop body.
  EXPECT_TRUE(VPDT.dominates(R1BB1, R1BB2));
  EXPECT_TRUE(VPDT.dominates(R1BB1, R1BB3));
  EXPECT_TRUE(VPDT.dominates(R1BB2, R1BB3));
  EXPECT_FALSE(VPDT.dominates(R1BB3, R1BB2));
  EXPECT_FALSE(VPDT.dominates(R1BB2, R1BB1));
}

} // namespace
} // namespace llvm
