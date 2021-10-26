/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 14:48:36 UTC 2021
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
      double[][] doubleArray0 = new double[2][7];
      double[] doubleArray1 = new double[1];
      doubleArray1[0] = 1480.4;
      doubleArray0[0] = doubleArray1;
      double[] doubleArray2 = new double[1];
      doubleArray2[0] = 1480.4;
      doubleArray0[1] = doubleArray2;
      BigMatrixImpl bigMatrixImpl0 = new BigMatrixImpl(doubleArray0);
      BigDecimal bigDecimal0 = bigMatrixImpl0.getEntry(0, 0);
      BigDecimal[] bigDecimalArray0 = new BigDecimal[1];
      bigDecimalArray0[0] = bigDecimal0;
      BigMatrixImpl bigMatrixImpl1 = new BigMatrixImpl(bigDecimalArray0);
      BigDecimal[] bigDecimalArray1 = new BigDecimal[4];
      bigDecimalArray1[0] = bigDecimal0;
      bigDecimalArray1[1] = bigDecimal0;
      bigDecimalArray1[2] = bigDecimal0;
      bigDecimalArray1[3] = bigDecimal0;
      BigMatrixImpl bigMatrixImpl2 = new BigMatrixImpl(bigDecimalArray1);
      // Undeclared exception!
      bigMatrixImpl2.operate(bigDecimalArray0);
  }
}
