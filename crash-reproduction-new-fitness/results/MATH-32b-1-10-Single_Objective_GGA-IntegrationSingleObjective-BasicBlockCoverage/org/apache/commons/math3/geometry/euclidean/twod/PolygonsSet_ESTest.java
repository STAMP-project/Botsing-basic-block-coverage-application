/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:20:50 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.twod;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PolygonsSet_ESTest extends PolygonsSet_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BSPTree<Euclidean2D> bSPTree0 = new BSPTree<Euclidean2D>();
      bSPTree0.copySelf();
      Object object0 = new Object();
      Euclidean1D.getInstance();
      bSPTree0.setAttribute(object0);
      PolygonsSet polygonsSet0 = new PolygonsSet();
      PolygonsSet polygonsSet1 = polygonsSet0.buildNew(bSPTree0);
      polygonsSet0.computeGeometricalProperties();
      // Undeclared exception!
      polygonsSet1.computeGeometricalProperties();
  }
}
