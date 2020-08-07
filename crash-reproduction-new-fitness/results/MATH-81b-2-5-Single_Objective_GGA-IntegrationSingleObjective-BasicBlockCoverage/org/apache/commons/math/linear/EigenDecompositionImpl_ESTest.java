/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:45:27 UTC 2020
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
      doubleArray0[0] = 0.0;
      doubleArray0[1] = 6.0;
      doubleArray0[2] = 2.0;
      doubleArray0[4] = 987.9895;
      doubleArray0[5] = (-5620.6);
      doubleArray0[7] = (-1289.297425936305);
      doubleArray0[8] = 27.0;
      double[] doubleArray1 = new double[8];
      doubleArray1[1] = 2.0;
      doubleArray1[2] = (-5620.6);
      doubleArray1[3] = 1187.8;
      doubleArray1[4] = 27.0;
      doubleArray1[6] = 1187.8;
      doubleArray1[7] = (-1289.297425936305);
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 0.0);
  }
}
