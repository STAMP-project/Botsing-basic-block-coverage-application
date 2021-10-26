/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 14:47:13 UTC 2021
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
      doubleArray0[0] = (-0.00395067576694);
      doubleArray0[1] = (-0.00395067576694);
      doubleArray0[2] = (-0.00395067576694);
      doubleArray0[3] = (-0.00395067576694);
      doubleArray0[4] = 349.46397833661;
      doubleArray0[5] = (-0.00395067576694);
      doubleArray0[6] = 0.9092974268256817;
      doubleArray0[7] = (-0.00395067576694);
      doubleArray0[8] = (-0.00395067576694);
      double[] doubleArray1 = new double[8];
      doubleArray1[0] = 0.9092974268256817;
      doubleArray1[1] = 7.741709714074981;
      doubleArray1[2] = 349.46397833661;
      doubleArray1[3] = 349.46397833661;
      doubleArray1[4] = 0.5235987755982989;
      doubleArray1[5] = 349.46397833661;
      doubleArray1[6] = 349.46397833661;
      doubleArray1[7] = 349.46397833661;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 0.9092974268256817);
  }
}
