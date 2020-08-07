/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:47:10 UTC 2020
 */

package org.apache.commons.math3.geometry.partitioning;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D;
import org.apache.commons.math3.geometry.euclidean.oned.OrientedPoint;
import org.apache.commons.math3.geometry.euclidean.oned.SubOrientedPoint;
import org.apache.commons.math3.geometry.euclidean.oned.Vector1D;
import org.apache.commons.math3.geometry.euclidean.threed.PolyhedronsSet;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.apache.commons.math3.geometry.euclidean.twod.Line;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.euclidean.twod.SubLine;
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.apache.commons.math3.geometry.partitioning.Region;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractRegion_ESTest extends AbstractRegion_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PolygonsSet polygonsSet0 = new PolygonsSet();
      Vector2D vector2D0 = new Vector2D((-1935.0582963767722), (-1935.0582963767722));
      Line line0 = new Line(vector2D0, (-1935.0582963767722));
      PolyhedronsSet polyhedronsSet0 = new PolyhedronsSet();
      PolyhedronsSet polyhedronsSet1 = new PolyhedronsSet();
      Line line1 = new Line(line0);
      Vector1D vector1D0 = Vector1D.NEGATIVE_INFINITY;
      boolean boolean0 = false;
      OrientedPoint orientedPoint0 = new OrientedPoint(vector1D0, false);
      SubOrientedPoint subOrientedPoint0 = orientedPoint0.wholeHyperplane();
      subOrientedPoint0.getRemainingRegion();
      SubLine subLine0 = new SubLine(line1, (Region<Euclidean1D>) null);
      SubOrientedPoint subOrientedPoint1 = orientedPoint0.wholeHyperplane();
      BSPTree<Euclidean2D> bSPTree0 = new BSPTree<Euclidean2D>(subOrientedPoint1);
      bSPTree0.split(subLine0);
      orientedPoint0.wholeHyperplane();
      PolygonsSet polygonsSet1 = new PolygonsSet(bSPTree0);
      // Undeclared exception!
      polygonsSet1.getSize();
  }
}
