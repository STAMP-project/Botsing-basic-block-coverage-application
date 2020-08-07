/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:14:10 UTC 2020
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
      BigDecimal[] bigDecimalArray0 = new BigDecimal[14];
      BigDecimal[][] bigDecimalArray1 = new BigDecimal[1][7];
      bigDecimalArray1[0] = bigDecimalArray0;
      BigDecimal[] bigDecimalArray2 = new BigDecimal[1];
      BigDecimal bigDecimal0 = BigDecimal.ZERO;
      bigDecimalArray2[0] = bigDecimal0;
      BigMatrixImpl bigMatrixImpl0 = new BigMatrixImpl(bigDecimalArray2);
      BigDecimal[] bigDecimalArray3 = new BigDecimal[5];
      bigDecimalArray3[0] = bigDecimal0;
      bigDecimalArray3[1] = bigDecimal0;
      bigDecimalArray3[2] = bigDecimal0;
      bigDecimalArray3[3] = bigDecimal0;
      bigDecimalArray3[4] = bigDecimal0;
      BigMatrixImpl bigMatrixImpl1 = new BigMatrixImpl(bigDecimalArray3);
      bigMatrixImpl1.hashCode();
      BigDecimal[] bigDecimalArray4 = new BigDecimal[1];
      bigDecimalArray4[0] = bigDecimal0;
      // Undeclared exception!
      bigMatrixImpl1.operate(bigDecimalArray4);
  }
}
