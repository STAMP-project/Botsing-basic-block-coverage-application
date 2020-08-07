/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 11:48:57 UTC 2020
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
      BigDecimal bigDecimal0 = BigDecimal.TEN;
      BigDecimal bigDecimal1 = BigDecimal.TEN;
      BigDecimal bigDecimal2 = BigMatrixImpl.ONE;
      BigDecimal bigDecimal3 = BigDecimal.TEN;
      BigDecimal bigDecimal4 = BigDecimal.TEN;
      BigDecimal bigDecimal5 = BigDecimal.ZERO;
      BigDecimal[] bigDecimalArray0 = new BigDecimal[1];
      bigDecimalArray0[0] = bigDecimal4;
      BigDecimal[][] bigDecimalArray1 = new BigDecimal[6][9];
      bigDecimalArray1[0] = bigDecimalArray0;
      bigDecimalArray1[1] = bigDecimalArray0;
      bigDecimalArray1[2] = bigDecimalArray0;
      bigDecimalArray1[3] = bigDecimalArray0;
      bigDecimalArray1[4] = bigDecimalArray0;
      bigDecimalArray1[5] = bigDecimalArray0;
      BigMatrixImpl bigMatrixImpl0 = new BigMatrixImpl(bigDecimalArray1);
      // Undeclared exception!
      bigMatrixImpl0.operate(bigDecimalArray0);
  }
}
