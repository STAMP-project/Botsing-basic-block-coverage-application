/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:36:21 UTC 2020
 */

package org.apache.commons.math3.geometry.partitioning;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.awt.geom.AffineTransform;
import org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D;
import org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet;
import org.apache.commons.math3.geometry.euclidean.threed.Plane;
import org.apache.commons.math3.geometry.euclidean.threed.RotationOrder;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.apache.commons.math3.geometry.euclidean.twod.Line;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.euclidean.twod.SubLine;
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.apache.commons.math3.geometry.partitioning.Transform;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractRegion_ESTest extends AbstractRegion_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      IntervalsSet intervalsSet0 = new IntervalsSet();
      intervalsSet0.copySelf();
      intervalsSet0.getSize();
      RotationOrder rotationOrder0 = RotationOrder.YZX;
      Vector3D vector3D0 = rotationOrder0.getA3();
      Plane plane0 = new Plane(vector3D0, vector3D0);
      Vector2D vector2D0 = plane0.toSubSpace(vector3D0);
      Vector2D vector2D1 = new Vector2D(Double.POSITIVE_INFINITY, vector2D0);
      Line line0 = new Line(vector2D1, Double.POSITIVE_INFINITY);
      PolygonsSet polygonsSet0 = line0.wholeSpace();
      polygonsSet0.getSize();
      AffineTransform affineTransform0 = AffineTransform.getRotateInstance(Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY, 1862.82);
      Transform<Euclidean2D, Euclidean1D> transform0 = Line.getTransform(affineTransform0);
      polygonsSet0.applyTransform(transform0);
      IntervalsSet intervalsSet1 = new IntervalsSet((BSPTree<Euclidean1D>) null);
      Line line1 = new Line(vector2D1, vector2D0);
      SubLine subLine0 = line1.wholeHyperplane();
      polygonsSet0.intersection(subLine0);
      polygonsSet0.getSize();
      IntervalsSet intervalsSet2 = new IntervalsSet((BSPTree<Euclidean1D>) null);
      IntervalsSet intervalsSet3 = new IntervalsSet(Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY);
      intervalsSet3.asList();
      IntervalsSet intervalsSet4 = new IntervalsSet((-3472.7466949891), Double.POSITIVE_INFINITY);
      Boolean boolean0 = Boolean.valueOf("tEX!8 o");
      BSPTree<Euclidean2D> bSPTree0 = new BSPTree<Euclidean2D>(boolean0);
      BSPTree<Euclidean2D> bSPTree1 = bSPTree0.split(subLine0);
      PolygonsSet polygonsSet1 = new PolygonsSet(bSPTree1);
      // Undeclared exception!
      polygonsSet1.getSize();
  }
}
