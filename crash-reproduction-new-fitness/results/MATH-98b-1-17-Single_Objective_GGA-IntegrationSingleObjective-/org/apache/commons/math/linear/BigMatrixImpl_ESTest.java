/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:13:50 UTC 2020
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
      BigDecimal[] bigDecimalArray0 = new BigDecimal[1];
      BigDecimal bigDecimal0 = BigDecimal.ZERO;
      bigDecimalArray0[0] = bigDecimal0;
      BigDecimal bigDecimal1 = new BigDecimal(2919L);
      BigDecimal[] bigDecimalArray1 = new BigDecimal[9];
      BigDecimal bigDecimal2 = BigMatrixImpl.ZERO;
      bigDecimalArray1[0] = bigDecimal2;
      bigDecimalArray1[1] = bigDecimal0;
      bigDecimalArray1[2] = bigDecimal1;
      bigDecimalArray1[3] = bigDecimal1;
      bigDecimalArray1[4] = bigDecimal0;
      bigDecimalArray1[5] = bigDecimal0;
      bigDecimalArray1[6] = bigDecimal0;
      BigDecimal bigDecimal3 = BigDecimal.ZERO;
      bigDecimalArray1[7] = bigDecimal3;
      bigDecimalArray1[8] = bigDecimal0;
      BigMatrixImpl bigMatrixImpl0 = new BigMatrixImpl(bigDecimalArray1);
      bigMatrixImpl0.copy();
      bigDecimal3.signum();
      BigDecimal bigDecimal4 = BigMatrixImpl.ZERO;
      // Undeclared exception!
      bigMatrixImpl0.operate(bigDecimalArray0);
  }
}
