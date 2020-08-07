/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:50:34 UTC 2020
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
      BigDecimal bigDecimal0 = BigDecimal.ONE;
      bigDecimalArray0[0] = bigDecimal0;
      BigDecimal[][] bigDecimalArray1 = new BigDecimal[2][3];
      bigDecimalArray1[0] = bigDecimalArray0;
      bigDecimalArray1[1] = bigDecimalArray0;
      BigMatrixImpl bigMatrixImpl0 = new BigMatrixImpl(bigDecimalArray1);
      BigMatrixImpl bigMatrixImpl1 = new BigMatrixImpl(bigDecimalArray1, false);
      // Undeclared exception!
      bigMatrixImpl1.operate(bigDecimalArray0);
  }
}
