/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:11:39 UTC 2020
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
      double[] doubleArray0 = new double[7];
      doubleArray0[0] = 0.0;
      doubleArray0[1] = 0.0;
      doubleArray0[2] = 4.0;
      doubleArray0[3] = (-3531.0);
      doubleArray0[4] = 0.9999999999999998;
      Double.valueOf(0.25);
      double[] doubleArray1 = new double[8];
      doubleArray1[0] = 4.0;
      doubleArray1[1] = (-3531.0);
      doubleArray1[2] = 4.0;
      doubleArray1[3] = 0.0;
      doubleArray1[4] = 0.25;
      doubleArray1[5] = 0.9999999999999998;
      doubleArray1[6] = 4.0;
      doubleArray1[7] = 0.9999999999999998;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray1, doubleArray0, 0.4636476090008061);
  }
}
