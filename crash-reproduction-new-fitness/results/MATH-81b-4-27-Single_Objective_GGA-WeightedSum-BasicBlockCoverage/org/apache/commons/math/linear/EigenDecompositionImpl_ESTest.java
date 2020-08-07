/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:12:53 UTC 2020
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
      double[][] doubleArray0 = new double[7][9];
      double[] doubleArray1 = new double[4];
      doubleArray1[0] = 4.0656970491754025;
      doubleArray1[1] = 66.663277012265;
      doubleArray1[2] = 4.0656970491754025;
      double[] doubleArray2 = new double[3];
      doubleArray2[0] = (double) (-8);
      doubleArray2[1] = 66.663277012265;
      doubleArray2[2] = (double) (-8);
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray1, doubleArray2, 4.0656970491754025);
      EigenDecompositionImpl eigenDecompositionImpl1 = new EigenDecompositionImpl(doubleArray1, doubleArray2, 1.232595164407831E-28);
  }
}
