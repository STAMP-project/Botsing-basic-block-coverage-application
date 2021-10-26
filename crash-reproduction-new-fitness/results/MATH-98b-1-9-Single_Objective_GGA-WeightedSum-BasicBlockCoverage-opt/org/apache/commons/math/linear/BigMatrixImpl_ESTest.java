/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 14:48:58 UTC 2021
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
      BigDecimal bigDecimal0 = new BigDecimal(17);
      bigDecimalArray0[0] = bigDecimal0;
      BigMatrixImpl bigMatrixImpl0 = new BigMatrixImpl(bigDecimalArray0);
      BigDecimal[] bigDecimalArray1 = bigMatrixImpl0.operate(bigDecimalArray0);
      BigDecimal bigDecimal1 = bigDecimal0.remainder(bigDecimal0);
      bigMatrixImpl0.solve(bigDecimalArray1);
      bigDecimal0.multiply(bigDecimal1);
      BigDecimal[] bigDecimalArray2 = bigMatrixImpl0.operate(bigDecimalArray0);
      bigMatrixImpl0.getTrace();
      BigDecimal bigDecimal2 = BigDecimal.TEN;
      BigDecimal[][] bigDecimalArray3 = new BigDecimal[3][0];
      bigDecimalArray3[0] = bigDecimalArray2;
      bigDecimalArray3[1] = bigDecimalArray0;
      bigDecimalArray3[2] = bigDecimalArray0;
      BigMatrixImpl bigMatrixImpl1 = new BigMatrixImpl(bigDecimalArray3);
      // Undeclared exception!
      bigMatrixImpl1.operate(bigDecimalArray0);
  }
}
