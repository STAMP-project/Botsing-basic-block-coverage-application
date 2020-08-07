/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:40:21 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.twod;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.awt.geom.AffineTransform;
import org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.apache.commons.math3.geometry.euclidean.twod.Line;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.apache.commons.math3.geometry.partitioning.Transform;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PolygonsSet_ESTest extends PolygonsSet_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BSPTree<Euclidean2D> bSPTree0 = (BSPTree<Euclidean2D>) mock(BSPTree.class, new ViolatedAssumptionAnswer());
      PolygonsSet polygonsSet0 = new PolygonsSet();
      polygonsSet0.getTree(false);
      polygonsSet0.getBoundarySize();
      polygonsSet0.computeGeometricalProperties();
      polygonsSet0.getBoundarySize();
      polygonsSet0.getVertices();
      AffineTransform affineTransform0 = AffineTransform.getRotateInstance((-2506.330352837), (-2506.330352837), (-3607.735968228), (-3607.735968228));
      Transform<Euclidean2D, Euclidean1D> transform0 = Line.getTransform(affineTransform0);
      polygonsSet0.applyTransform(transform0);
      polygonsSet0.getVertices();
      polygonsSet0.isEmpty();
      BSPTree<Euclidean2D> bSPTree1 = new BSPTree<Euclidean2D>("");
      PolygonsSet polygonsSet1 = polygonsSet0.buildNew(bSPTree1);
      polygonsSet0.getTree(false);
      polygonsSet0.isEmpty();
      // Undeclared exception!
      polygonsSet1.computeGeometricalProperties();
  }
}
