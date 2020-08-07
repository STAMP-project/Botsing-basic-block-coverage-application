/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:44:47 UTC 2020
 */

package org.apache.commons.math3.geometry.partitioning;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D;
import org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet;
import org.apache.commons.math3.geometry.euclidean.threed.PolyhedronsSet;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.euclidean.twod.SubLine;
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.apache.commons.math3.geometry.partitioning.BoundarySizeVisitor;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractRegion_ESTest extends AbstractRegion_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Object object0 = new Object();
      BoundarySizeVisitor<Euclidean1D> boundarySizeVisitor0 = new BoundarySizeVisitor<Euclidean1D>();
      Vector3D vector3D0 = Vector3D.ZERO;
      PolyhedronsSet polyhedronsSet0 = new PolyhedronsSet();
      PolygonsSet polygonsSet0 = new PolygonsSet();
      polygonsSet0.getSize();
      IntervalsSet intervalsSet0 = new IntervalsSet();
      BoundarySizeVisitor<Euclidean1D> boundarySizeVisitor1 = new BoundarySizeVisitor<Euclidean1D>();
      Vector3D vector3D1 = Vector3D.ZERO;
      PolyhedronsSet polyhedronsSet1 = new PolyhedronsSet();
      PolygonsSet polygonsSet1 = new PolygonsSet();
      Integer integer0 = new Integer((-1));
      BSPTree<Euclidean2D> bSPTree0 = new BSPTree<Euclidean2D>(integer0);
      Vector2D vector2D0 = Vector2D.POSITIVE_INFINITY;
      SubLine subLine0 = new SubLine(vector2D0, vector2D0);
      bSPTree0.split(subLine0);
      PolygonsSet polygonsSet2 = new PolygonsSet(bSPTree0);
      // Undeclared exception!
      polygonsSet2.getSize();
  }
}
