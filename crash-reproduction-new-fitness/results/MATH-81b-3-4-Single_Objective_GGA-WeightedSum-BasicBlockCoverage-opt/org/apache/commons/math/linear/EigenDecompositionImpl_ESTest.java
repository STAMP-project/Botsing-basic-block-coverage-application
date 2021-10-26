/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 14:47:19 UTC 2021
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
      double[] doubleArray0 = new double[8];
      doubleArray0[1] = 1593.038644960088;
      doubleArray0[2] = 781.0;
      double double0 = 1.718281828459045;
      doubleArray0[1] = 1.718281828459045;
      doubleArray0[4] = 2.0779220779220777;
      doubleArray0[5] = 781.0;
      doubleArray0[7] = 781.0;
      double[] doubleArray1 = new double[7];
      doubleArray1[6] = 1416.4;
      doubleArray1[1] = 1416.4;
      doubleArray1[2] = 1.718281828459045;
      doubleArray1[3] = 1416.4;
      doubleArray1[4] = 1416.4;
      doubleArray1[5] = 1416.4;
      doubleArray1[6] = 1593.038644960088;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 0.0);
  }
}
