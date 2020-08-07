/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:44:21 UTC 2020
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
      doubleArray0[0] = (-1199.860031793);
      doubleArray0[1] = 940.45349484286;
      doubleArray0[2] = 1048.0;
      doubleArray0[3] = 0.0;
      doubleArray0[4] = 6.38905609893065;
      doubleArray0[5] = (-2440.4366047046074);
      doubleArray0[6] = (-1855.60125798);
      double[] doubleArray1 = new double[8];
      doubleArray1[0] = 1262.71149879073;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray1, doubleArray0, 6.38905609893065);
  }
}
