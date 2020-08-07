/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:08:28 UTC 2020
 */

package org.apache.commons.math.linear;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.linear.DefaultRealMatrixChangingVisitor;
import org.apache.commons.math.linear.EigenDecompositionImpl;
import org.apache.commons.math.linear.OpenMapRealMatrix;
import org.apache.commons.math.linear.RealMatrixChangingVisitor;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EigenDecompositionImpl_ESTest extends EigenDecompositionImpl_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      int int0 = 12;
      OpenMapRealMatrix openMapRealMatrix0 = new OpenMapRealMatrix(12, 12);
      DefaultRealMatrixChangingVisitor defaultRealMatrixChangingVisitor0 = new DefaultRealMatrixChangingVisitor();
      openMapRealMatrix0.walkInColumnOrder((RealMatrixChangingVisitor) defaultRealMatrixChangingVisitor0);
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(openMapRealMatrix0, 12);
      double double0 = (-3128.35447302);
      double[] doubleArray0 = new double[5];
      doubleArray0[0] = 624.3417889454365;
      doubleArray0[1] = (double) 12;
      doubleArray0[2] = 2252.5594801868783;
      doubleArray0[0] = 1.2599210498948732;
      doubleArray0[4] = 1.2599210498948732;
      double[] doubleArray1 = new double[4];
      doubleArray0[2] = (-3128.35447302);
      doubleArray1[1] = (double) 12;
      doubleArray1[2] = (double) 12;
      doubleArray1[3] = 624.3417889454365;
      EigenDecompositionImpl eigenDecompositionImpl1 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 0.0);
  }
}
