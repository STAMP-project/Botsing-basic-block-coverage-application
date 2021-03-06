/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:13:50 UTC 2020
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
      double[] doubleArray0 = new double[8];
      doubleArray0[1] = (double) 2147483130;
      doubleArray0[2] = (double) 2147483130;
      doubleArray0[1] = (double) 2147483130;
      double[] doubleArray1 = new double[7];
      doubleArray1[1] = (-0.9899924966004454);
      doubleArray1[2] = (double) 2147483130;
      doubleArray1[4] = (-0.9899924966004454);
      doubleArray1[5] = (double) 2147483130;
      doubleArray1[6] = 2.14748313E9;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 2.14748313E9);
  }
}
