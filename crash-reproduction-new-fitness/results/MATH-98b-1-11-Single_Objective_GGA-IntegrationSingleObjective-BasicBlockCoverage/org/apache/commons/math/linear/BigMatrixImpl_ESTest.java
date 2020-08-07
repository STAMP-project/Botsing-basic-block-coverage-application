/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:52:47 UTC 2020
 */

package org.apache.commons.math.linear;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.math.BigDecimal;
import java.math.MathContext;
import org.apache.commons.math.linear.BigMatrixImpl;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BigMatrixImpl_ESTest extends BigMatrixImpl_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BigDecimal[][] bigDecimalArray0 = new BigDecimal[12][2];
      BigDecimal[] bigDecimalArray1 = new BigDecimal[1];
      BigDecimal bigDecimal0 = BigMatrixImpl.ONE;
      BigDecimal bigDecimal1 = BigDecimal.ONE;
      BigDecimal[] bigDecimalArray2 = bigDecimal0.divideAndRemainder(bigDecimal1);
      bigDecimalArray1[0] = bigDecimal0;
      bigDecimalArray0[0] = bigDecimalArray1;
      bigDecimal0.byteValueExact();
      MathContext mathContext0 = MathContext.DECIMAL128;
      BigDecimal bigDecimal2 = BigMatrixImpl.ZERO;
      double[][] doubleArray0 = new double[1][0];
      BigDecimal bigDecimal3 = bigDecimal1.divideToIntegralValue(bigDecimal0);
      double[] doubleArray1 = new double[1];
      doubleArray1[0] = (double) (-1);
      doubleArray0[0] = doubleArray1;
      BigMatrixImpl bigMatrixImpl0 = new BigMatrixImpl(doubleArray0);
      bigMatrixImpl0.operate(bigDecimalArray1);
      bigMatrixImpl0.getTrace();
      MathContext mathContext1 = MathContext.UNLIMITED;
      bigDecimal0.subtract(bigDecimal3, mathContext1);
      BigMatrixImpl bigMatrixImpl1 = new BigMatrixImpl(doubleArray0);
      bigMatrixImpl0.setScale(5);
      BigMatrixImpl bigMatrixImpl2 = new BigMatrixImpl(bigDecimalArray2);
      bigMatrixImpl1.copy();
      bigMatrixImpl2.getColumnDimension();
      // Undeclared exception!
      bigMatrixImpl2.operate(bigDecimalArray1);
  }
}
