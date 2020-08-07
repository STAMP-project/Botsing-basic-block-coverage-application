/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 11:51:19 UTC 2020
 */

package org.apache.commons.math.linear;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.linear.Array2DRowRealMatrix;
import org.apache.commons.math.linear.EigenDecompositionImpl;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EigenDecompositionImpl_ESTest extends EigenDecompositionImpl_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Array2DRowRealMatrix array2DRowRealMatrix0 = new Array2DRowRealMatrix();
      double[] doubleArray0 = new double[5];
      doubleArray0[0] = 0.333;
      doubleArray0[2] = 3002.7;
      doubleArray0[3] = 0.333;
      doubleArray0[4] = (-811.2903922);
      double[] doubleArray1 = new double[4];
      doubleArray1[0] = 0.333;
      doubleArray1[1] = (-1145.93773315);
      doubleArray1[2] = 3002.7;
      doubleArray1[3] = 3002.7;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 0.333);
  }
}
