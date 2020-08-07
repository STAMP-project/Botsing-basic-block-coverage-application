/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:16:37 UTC 2020
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
      double double0 = 4.0;
      int int0 = 1022;
      double double1 = 0.5292152899175504;
      double[] doubleArray0 = new double[6];
      doubleArray0[0] = (double) 1022;
      double[] doubleArray1 = new double[5];
      doubleArray1[0] = 0.9;
      doubleArray1[2] = 0.5292152899175504;
      doubleArray1[3] = (double) 1022;
      doubleArray1[4] = 1621.414889564;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 1022.0);
  }
}
