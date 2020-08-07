/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:50:41 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.twod;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.LinkedList;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.apache.commons.math3.geometry.partitioning.SubHyperplane;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PolygonsSet_ESTest extends PolygonsSet_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      LinkedList<SubHyperplane<Euclidean2D>> linkedList0 = new LinkedList<SubHyperplane<Euclidean2D>>();
      PolygonsSet polygonsSet0 = new PolygonsSet(linkedList0);
      PolygonsSet polygonsSet1 = new PolygonsSet();
      polygonsSet1.computeGeometricalProperties();
      polygonsSet1.computeGeometricalProperties();
      polygonsSet0.computeGeometricalProperties();
      polygonsSet1.computeGeometricalProperties();
      polygonsSet1.computeGeometricalProperties();
      Object object0 = new Object();
      BSPTree<Euclidean2D> bSPTree0 = new BSPTree<Euclidean2D>(object0);
      PolygonsSet polygonsSet2 = polygonsSet1.buildNew(bSPTree0);
      polygonsSet0.computeGeometricalProperties();
      // Undeclared exception!
      polygonsSet2.computeGeometricalProperties();
  }
}
