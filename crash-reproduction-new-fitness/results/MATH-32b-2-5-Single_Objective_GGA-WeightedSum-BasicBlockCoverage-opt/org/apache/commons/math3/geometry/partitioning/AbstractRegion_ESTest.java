/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 15:10:51 UTC 2021
 */

package org.apache.commons.math3.geometry.partitioning;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D;
import org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet;
import org.apache.commons.math3.geometry.euclidean.threed.Euclidean3D;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.apache.commons.math3.geometry.partitioning.Transform;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractRegion_ESTest extends AbstractRegion_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BSPTree<Euclidean1D> bSPTree0 = new BSPTree<Euclidean1D>((Object) null);
      bSPTree0.getMinus();
      IntervalsSet intervalsSet0 = new IntervalsSet(bSPTree0);
      Transform<Euclidean3D, Euclidean2D> transform0 = (Transform<Euclidean3D, Euclidean2D>) mock(Transform.class, new ViolatedAssumptionAnswer());
      BSPTree<Euclidean2D> bSPTree1 = new BSPTree<Euclidean2D>(transform0);
      PolygonsSet polygonsSet0 = new PolygonsSet(bSPTree1);
      // Undeclared exception!
      polygonsSet0.getSize();
  }
}
