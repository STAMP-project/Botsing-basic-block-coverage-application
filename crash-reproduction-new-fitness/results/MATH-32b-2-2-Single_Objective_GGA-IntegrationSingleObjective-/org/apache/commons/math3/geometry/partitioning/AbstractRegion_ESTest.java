/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:15:49 UTC 2020
 */

package org.apache.commons.math3.geometry.partitioning;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Comparator;
import org.apache.commons.math3.geometry.euclidean.threed.Euclidean3D;
import org.apache.commons.math3.geometry.euclidean.threed.RotationOrder;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.euclidean.twod.SubLine;
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.apache.commons.math3.geometry.partitioning.BoundarySizeVisitor;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractRegion_ESTest extends AbstractRegion_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BSPTree<Euclidean3D> bSPTree0 = new BSPTree<Euclidean3D>();
      BSPTree<Euclidean3D> bSPTree1 = bSPTree0.copySelf();
      BoundarySizeVisitor<Euclidean3D> boundarySizeVisitor0 = new BoundarySizeVisitor<Euclidean3D>();
      BSPTree.LeafMerger<Euclidean3D> bSPTree_LeafMerger0 = (BSPTree.LeafMerger<Euclidean3D>) mock(BSPTree.LeafMerger.class, new ViolatedAssumptionAnswer());
      doReturn((BSPTree) null).when(bSPTree_LeafMerger0).merge(nullable(org.apache.commons.math3.geometry.partitioning.BSPTree.class) , nullable(org.apache.commons.math3.geometry.partitioning.BSPTree.class) , nullable(org.apache.commons.math3.geometry.partitioning.BSPTree.class) , anyBoolean() , anyBoolean());
      bSPTree0.merge(bSPTree1, bSPTree_LeafMerger0);
      Comparator<Object> comparator0 = (Comparator<Object>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      PolygonsSet polygonsSet0 = new PolygonsSet();
      BSPTree<Euclidean3D> bSPTree2 = bSPTree1.copySelf();
      Vector2D vector2D0 = Vector2D.NEGATIVE_INFINITY;
      vector2D0.negate();
      vector2D0.getNorm();
      SubLine subLine0 = new SubLine(vector2D0, vector2D0);
      bSPTree1.setAttribute(vector2D0);
      RotationOrder rotationOrder0 = RotationOrder.YZX;
      Vector3D vector3D0 = Vector3D.MINUS_I;
      bSPTree1.getPlus();
      double double0 = 3.0;
      BSPTree<Euclidean2D> bSPTree3 = new BSPTree<Euclidean2D>(bSPTree2);
      BSPTree<Euclidean2D> bSPTree4 = bSPTree3.copySelf();
      PolygonsSet polygonsSet1 = polygonsSet0.buildNew(bSPTree4);
      // Undeclared exception!
      polygonsSet1.getSize();
  }
}
