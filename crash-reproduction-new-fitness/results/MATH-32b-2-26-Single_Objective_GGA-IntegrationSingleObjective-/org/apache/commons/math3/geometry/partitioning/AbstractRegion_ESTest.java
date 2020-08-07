/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:47:03 UTC 2020
 */

package org.apache.commons.math3.geometry.partitioning;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.LinkedList;
import java.util.Locale;
import org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D;
import org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractRegion_ESTest extends AbstractRegion_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BSPTree<Euclidean1D> bSPTree0 = (BSPTree<Euclidean1D>) mock(BSPTree.class, new ViolatedAssumptionAnswer());
      IntervalsSet intervalsSet0 = new IntervalsSet();
      LinkedList<Locale.LanguageRange> linkedList0 = new LinkedList<Locale.LanguageRange>();
      PolygonsSet polygonsSet0 = new PolygonsSet();
      polygonsSet0.getSize();
      Object object0 = new Object();
      BSPTree<Euclidean2D> bSPTree1 = new BSPTree<Euclidean2D>(object0);
      Vector2D vector2D0 = Vector2D.POSITIVE_INFINITY;
      Vector2D vector2D1 = new Vector2D(Double.POSITIVE_INFINITY, vector2D0, Double.POSITIVE_INFINITY, vector2D0, Double.POSITIVE_INFINITY, vector2D0);
      BSPTree<Euclidean2D> bSPTree2 = bSPTree1.getCell(vector2D1);
      BSPTree<Euclidean2D> bSPTree3 = bSPTree2.copySelf();
      PolygonsSet polygonsSet1 = polygonsSet0.buildNew(bSPTree3);
      // Undeclared exception!
      polygonsSet1.getSize();
  }
}
