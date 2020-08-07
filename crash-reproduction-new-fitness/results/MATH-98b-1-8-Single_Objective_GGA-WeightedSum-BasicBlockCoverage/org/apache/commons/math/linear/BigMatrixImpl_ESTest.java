/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 11:48:30 UTC 2020
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
      BigDecimal[] bigDecimalArray0 = new BigDecimal[2];
      BigDecimal bigDecimal0 = BigDecimal.ONE;
      bigDecimalArray0[0] = bigDecimal0;
      BigDecimal bigDecimal1 = BigMatrixImpl.ZERO;
      bigDecimalArray0[1] = bigDecimal1;
      BigMatrixImpl bigMatrixImpl0 = new BigMatrixImpl(bigDecimalArray0);
      bigMatrixImpl0.getDataAsDoubleArray();
      BigDecimal[] bigDecimalArray1 = new BigDecimal[1];
      bigDecimalArray1[0] = bigDecimal1;
      BigMatrixImpl bigMatrixImpl1 = new BigMatrixImpl(bigDecimalArray1);
      bigMatrixImpl1.getDataAsDoubleArray();
      bigMatrixImpl1.copy();
      // Undeclared exception!
      bigMatrixImpl0.operate(bigDecimalArray1);
  }
}
