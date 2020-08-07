/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 12:25:43 UTC 2020
 */

package org.apache.commons.math3.geometry.partitioning;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D;
import org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet;
import org.apache.commons.math3.geometry.euclidean.threed.PolyhedronsSet;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractRegion_ESTest extends AbstractRegion_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PolyhedronsSet polyhedronsSet0 = new PolyhedronsSet();
      polyhedronsSet0.getSize();
      IntervalsSet intervalsSet0 = new IntervalsSet(0.0, 0.0);
      BSPTree<Euclidean1D> bSPTree0 = new BSPTree<Euclidean1D>(intervalsSet0);
      intervalsSet0.buildNew(bSPTree0);
      BSPTree<Euclidean2D> bSPTree1 = new BSPTree<Euclidean2D>(bSPTree0);
      polyhedronsSet0.copySelf();
      PolygonsSet polygonsSet0 = new PolygonsSet(bSPTree1);
      // Undeclared exception!
      polygonsSet0.getSize();
  }
}
