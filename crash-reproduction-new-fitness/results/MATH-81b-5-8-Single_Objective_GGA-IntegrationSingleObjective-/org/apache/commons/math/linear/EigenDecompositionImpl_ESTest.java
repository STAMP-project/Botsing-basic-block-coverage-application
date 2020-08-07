/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:48:10 UTC 2020
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
      int int0 = (-1);
      double[] doubleArray0 = new double[7];
      doubleArray0[0] = (double) (-1);
      doubleArray0[2] = 0.0;
      doubleArray0[3] = (double) (-1);
      doubleArray0[4] = 0.0;
      doubleArray0[5] = 0.0;
      doubleArray0[4] = (double) (-1);
      double[] doubleArray1 = new double[6];
      doubleArray1[0] = 1254.914759629067;
      doubleArray1[3] = 3.0;
      doubleArray1[4] = (-3925.1651133122577);
      doubleArray1[5] = 2405.742235929639;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, (-1.0918632124457324E9));
  }
}
