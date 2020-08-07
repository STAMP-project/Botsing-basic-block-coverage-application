/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:37:31 UTC 2020
 */

package org.apache.commons.math3.geometry.partitioning;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.LinkedList;
import org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D;
import org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet;
import org.apache.commons.math3.geometry.euclidean.threed.Euclidean3D;
import org.apache.commons.math3.geometry.euclidean.threed.SubPlane;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.apache.commons.math3.geometry.euclidean.twod.Line;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.euclidean.twod.SubLine;
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.apache.commons.math3.geometry.partitioning.Hyperplane;
import org.apache.commons.math3.geometry.partitioning.SubHyperplane;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractRegion_ESTest extends AbstractRegion_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Vector2D vector2D0 = Vector2D.ZERO;
      Vector2D vector2D1 = new Vector2D((-1074.9962), vector2D0, 1.0E-10, vector2D0, 1.0E-10, vector2D0, (-1074.9962), vector2D0);
      Line line0 = new Line(vector2D1, vector2D1);
      LinkedList<SubHyperplane<Euclidean2D>> linkedList0 = new LinkedList<SubHyperplane<Euclidean2D>>();
      PolygonsSet polygonsSet0 = new PolygonsSet(linkedList0);
      SubPlane subPlane0 = new SubPlane((Hyperplane<Euclidean3D>) null, polygonsSet0);
      BSPTree<Euclidean1D> bSPTree0 = new BSPTree<Euclidean1D>(subPlane0);
      IntervalsSet intervalsSet0 = new IntervalsSet(bSPTree0);
      SubLine subLine0 = new SubLine(line0, intervalsSet0);
      BSPTree<Euclidean2D> bSPTree1 = (BSPTree<Euclidean2D>) mock(BSPTree.class, new ViolatedAssumptionAnswer());
      doReturn("", "", (String) null).when(bSPTree1).toString();
      doReturn("").when(bSPTree1).getAttribute();
      doReturn(subLine0, (SubHyperplane) null).when(bSPTree1).getCut();
      PolygonsSet polygonsSet1 = new PolygonsSet(bSPTree1);
      // Undeclared exception!
      polygonsSet1.getSize();
  }
}
