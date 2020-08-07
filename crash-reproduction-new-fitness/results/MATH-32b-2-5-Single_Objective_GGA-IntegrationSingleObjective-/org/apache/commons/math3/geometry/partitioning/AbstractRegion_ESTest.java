/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:18:58 UTC 2020
 */

package org.apache.commons.math3.geometry.partitioning;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.awt.geom.AffineTransform;
import org.apache.commons.math3.geometry.Vector;
import org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D;
import org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet;
import org.apache.commons.math3.geometry.euclidean.oned.Vector1D;
import org.apache.commons.math3.geometry.euclidean.threed.Euclidean3D;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.apache.commons.math3.geometry.euclidean.twod.Line;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.partitioning.AbstractRegion;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.apache.commons.math3.geometry.partitioning.Transform;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractRegion_ESTest extends AbstractRegion_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BSPTree<Euclidean2D> bSPTree0 = new BSPTree<Euclidean2D>();
      IntervalsSet intervalsSet0 = new IntervalsSet();
      IntervalsSet intervalsSet1 = new IntervalsSet();
      Vector1D vector1D0 = Vector1D.POSITIVE_INFINITY;
      Vector3D vector3D0 = Vector3D.NaN;
      Vector3D vector3D1 = vector3D0.normalize();
      vector3D0.orthogonal();
      vector3D1.distanceInf((Vector<Euclidean3D>) vector3D0);
      Integer integer0 = new Integer(1716);
      bSPTree0.setAttribute(integer0);
      AffineTransform affineTransform0 = AffineTransform.getQuadrantRotateInstance(2);
      Transform<Euclidean2D, Euclidean1D> transform0 = Line.getTransform(affineTransform0);
      PolygonsSet polygonsSet0 = new PolygonsSet();
      PolygonsSet polygonsSet1 = new PolygonsSet(bSPTree0);
      AbstractRegion<Euclidean2D, Euclidean1D> abstractRegion0 = polygonsSet1.applyTransform(transform0);
      abstractRegion0.setBarycenter((Vector<Euclidean2D>) null);
      // Undeclared exception!
      polygonsSet1.getSize();
  }
}
