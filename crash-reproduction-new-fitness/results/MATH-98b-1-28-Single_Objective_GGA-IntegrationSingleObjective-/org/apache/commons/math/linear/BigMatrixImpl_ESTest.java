/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:14:49 UTC 2020
 */

package org.apache.commons.math.linear;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.math.BigDecimal;
import org.apache.commons.math.linear.BigMatrixImpl;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BigMatrixImpl_ESTest extends BigMatrixImpl_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double[][] doubleArray0 = new double[1][8];
      double[] doubleArray1 = new double[1];
      doubleArray1[0] = (-52.94560776925817);
      doubleArray0[0] = doubleArray1;
      BigMatrixImpl bigMatrixImpl0 = new BigMatrixImpl(doubleArray0);
      BigMatrixImpl bigMatrixImpl1 = bigMatrixImpl0.subtract(bigMatrixImpl0);
      BigMatrixImpl bigMatrixImpl2 = bigMatrixImpl0.add(bigMatrixImpl1);
      BigDecimal[] bigDecimalArray0 = bigMatrixImpl2.operate(doubleArray1);
      bigMatrixImpl0.add(bigMatrixImpl1);
      BigDecimal[] bigDecimalArray1 = bigMatrixImpl2.operate(doubleArray1);
      BigDecimal[] bigDecimalArray2 = bigMatrixImpl2.operate(doubleArray1);
      BigDecimal[][] bigDecimalArray3 = new BigDecimal[7][5];
      bigDecimalArray3[0] = bigDecimalArray0;
      bigDecimalArray3[1] = bigDecimalArray0;
      bigDecimalArray3[2] = bigDecimalArray0;
      bigDecimalArray3[3] = bigDecimalArray0;
      bigDecimalArray3[4] = bigDecimalArray1;
      bigDecimalArray3[5] = bigDecimalArray1;
      bigDecimalArray3[6] = bigDecimalArray2;
      BigMatrixImpl bigMatrixImpl3 = new BigMatrixImpl(bigDecimalArray3, true);
      // Undeclared exception!
      bigMatrixImpl3.operate(bigDecimalArray2);
  }
}
