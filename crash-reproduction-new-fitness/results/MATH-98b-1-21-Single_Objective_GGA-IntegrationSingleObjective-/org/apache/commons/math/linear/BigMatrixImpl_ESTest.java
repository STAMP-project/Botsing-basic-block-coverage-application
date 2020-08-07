/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:15:25 UTC 2020
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
      BigDecimal[][] bigDecimalArray0 = new BigDecimal[0][9];
      BigMatrixImpl bigMatrixImpl0 = new BigMatrixImpl();
      BigDecimal bigDecimal0 = BigDecimal.ONE;
      BigDecimal[] bigDecimalArray1 = new BigDecimal[8];
      bigDecimalArray1[0] = bigDecimal0;
      BigDecimal bigDecimal1 = new BigDecimal(0L);
      bigDecimalArray1[1] = bigDecimal1;
      bigDecimalArray1[2] = bigDecimal0;
      bigDecimalArray1[3] = bigDecimal0;
      bigDecimalArray1[4] = bigDecimal0;
      BigDecimal bigDecimal2 = BigDecimal.TEN;
      bigDecimalArray1[5] = bigDecimal2;
      bigDecimalArray1[6] = bigDecimal0;
      bigDecimalArray1[7] = bigDecimal0;
      BigMatrixImpl bigMatrixImpl1 = new BigMatrixImpl(bigDecimalArray1);
      BigDecimal[] bigDecimalArray2 = new BigDecimal[1];
      bigDecimalArray2[0] = bigDecimal0;
      // Undeclared exception!
      bigMatrixImpl1.operate(bigDecimalArray2);
  }
}
