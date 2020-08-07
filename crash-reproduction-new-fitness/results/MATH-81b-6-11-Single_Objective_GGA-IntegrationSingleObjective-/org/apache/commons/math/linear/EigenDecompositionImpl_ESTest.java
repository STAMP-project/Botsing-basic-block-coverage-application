/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:51:19 UTC 2020
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
      double double0 = 2.220446049250313E-16;
      double[] doubleArray0 = new double[7];
      doubleArray0[0] = 2.220446049250313E-16;
      doubleArray0[1] = 2.220446049250313E-16;
      doubleArray0[2] = (-1437.41);
      doubleArray0[3] = (-2.579989129986133);
      doubleArray0[4] = (-2.579989129986133);
      doubleArray0[5] = 2.220446049250313E-16;
      double double1 = 744.75329911046;
      double[] doubleArray1 = new double[6];
      doubleArray1[0] = (-3.0);
      doubleArray1[1] = (-1437.41);
      doubleArray1[2] = (-1437.41);
      doubleArray1[3] = 0.24457744356470904;
      doubleArray1[4] = (-3.0);
      doubleArray1[5] = 2.220446049250313E-16;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 1.05);
  }
}
