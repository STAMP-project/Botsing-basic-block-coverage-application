/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:15:47 UTC 2020
 */

package org.apache.commons.math.linear;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.apache.commons.math.linear.BigMatrixImpl;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BigMatrixImpl_ESTest extends BigMatrixImpl_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BigDecimal[][] bigDecimalArray0 = new BigDecimal[1][0];
      BigDecimal[] bigDecimalArray1 = new BigDecimal[1];
      BigDecimal bigDecimal0 = BigDecimal.ONE;
      bigDecimal0.intValueExact();
      bigDecimalArray1[0] = bigDecimal0;
      bigDecimal0.longValue();
      BigDecimal bigDecimal1 = BigDecimal.ZERO;
      bigDecimalArray0[0] = bigDecimalArray1;
      bigDecimal0.toPlainString();
      BigDecimal bigDecimal2 = BigDecimal.ONE;
      BigDecimal bigDecimal3 = BigDecimal.ONE;
      BigInteger bigInteger0 = BigInteger.ZERO;
      BigDecimal bigDecimal4 = new BigDecimal(bigInteger0);
      BigDecimal[][] bigDecimalArray2 = new BigDecimal[4][8];
      bigDecimalArray2[0] = bigDecimalArray1;
      bigDecimalArray2[1] = bigDecimalArray1;
      bigDecimalArray2[2] = bigDecimalArray1;
      bigDecimalArray2[3] = bigDecimalArray1;
      BigMatrixImpl bigMatrixImpl0 = new BigMatrixImpl(bigDecimalArray2, false);
      // Undeclared exception!
      bigMatrixImpl0.operate(bigDecimalArray1);
  }
}
