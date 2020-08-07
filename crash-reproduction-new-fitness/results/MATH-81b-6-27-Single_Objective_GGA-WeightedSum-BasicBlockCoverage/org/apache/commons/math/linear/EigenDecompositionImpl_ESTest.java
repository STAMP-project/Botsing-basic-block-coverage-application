/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:14:26 UTC 2020
 */

package org.apache.commons.math.linear;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.linear.EigenDecompositionImpl;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EigenDecompositionImpl_ESTest extends EigenDecompositionImpl_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double[] doubleArray0 = new double[9];
      doubleArray0[0] = (-308400.3549750033);
      doubleArray0[1] = (double) 0;
      doubleArray0[2] = (double) 0;
      doubleArray0[3] = (double) 0;
      doubleArray0[4] = (double) 0;
      doubleArray0[5] = (double) 0;
      doubleArray0[6] = (double) 0;
      doubleArray0[7] = (double) 0;
      doubleArray0[8] = 594.813915;
      double[] doubleArray1 = new double[8];
      doubleArray1[0] = (-308400.3549750033);
      doubleArray1[1] = (double) 0;
      doubleArray1[2] = 594.813915;
      doubleArray1[3] = (-308400.3549750033);
      doubleArray1[4] = (-308400.3549750033);
      doubleArray1[5] = (double) 0;
      doubleArray1[6] = 594.813915;
      doubleArray1[7] = Double.POSITIVE_INFINITY;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 0.0);
  }
}
