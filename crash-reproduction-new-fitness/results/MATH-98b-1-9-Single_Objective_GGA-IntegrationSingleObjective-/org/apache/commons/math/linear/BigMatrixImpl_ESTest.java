/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:51:39 UTC 2020
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
      BigDecimal[][] bigDecimalArray0 = new BigDecimal[19][4];
      BigDecimal[] bigDecimalArray1 = new BigDecimal[1];
      BigDecimal bigDecimal0 = BigDecimal.ONE;
      bigDecimalArray1[0] = bigDecimal0;
      bigDecimalArray0[0] = bigDecimalArray1;
      BigDecimal[] bigDecimalArray2 = new BigDecimal[7];
      bigDecimalArray2[3] = bigDecimal0;
      bigDecimalArray2[1] = bigDecimal0;
      bigDecimalArray2[2] = bigDecimal0;
      bigDecimalArray2[4] = bigDecimal0;
      bigDecimalArray2[4] = bigDecimal0;
      BigDecimal bigDecimal1 = BigDecimal.ONE;
      bigDecimalArray2[5] = bigDecimal0;
      bigDecimalArray0[1] = bigDecimalArray1;
      BigDecimal[] bigDecimalArray3 = new BigDecimal[3];
      bigDecimalArray3[0] = bigDecimal0;
      bigDecimalArray3[1] = bigDecimal0;
      bigDecimalArray3[2] = bigDecimal0;
      BigDecimal bigDecimal2 = BigDecimal.ONE;
      BigMatrixImpl bigMatrixImpl0 = new BigMatrixImpl(bigDecimalArray3);
      bigMatrixImpl0.copy();
      bigMatrixImpl0.setScale(150);
      BigDecimal.valueOf((long) 150, 0);
      // Undeclared exception!
      bigMatrixImpl0.operate(bigDecimalArray1);
  }
}
