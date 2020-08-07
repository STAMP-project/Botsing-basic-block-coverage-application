/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:11:33 UTC 2020
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
      double[][] doubleArray0 = new double[4][1];
      double[] doubleArray1 = new double[8];
      doubleArray1[0] = 1293.6337118375122;
      doubleArray1[1] = 1175238.6088096087;
      doubleArray1[2] = 365.0383;
      doubleArray1[6] = 1293.6337118375122;
      doubleArray0[2] = doubleArray1;
      double[] doubleArray2 = new double[7];
      doubleArray2[0] = 169.79545291739862;
      doubleArray2[2] = 365.0383;
      doubleArray2[3] = 1175238.6088096087;
      doubleArray2[4] = 169.79545291739862;
      doubleArray2[5] = 0.0;
      Array2DRowRealMatrix array2DRowRealMatrix0 = new Array2DRowRealMatrix();
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray1, doubleArray2, 1.1102230246251565E-16);
  }
}
