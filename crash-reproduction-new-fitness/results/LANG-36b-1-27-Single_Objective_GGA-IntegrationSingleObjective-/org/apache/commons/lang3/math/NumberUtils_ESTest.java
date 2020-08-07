/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:58:40 UTC 2020
 */

package org.apache.commons.lang3.math;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.lang3.math.NumberUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class NumberUtils_ESTest extends NumberUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      NumberUtils.toLong("+g2wC", (-319L));
      NumberUtils.min(0.0F, 2716.6782F, 0.0F);
      NumberUtils.toDouble((String) null);
      long[] longArray0 = new long[1];
      longArray0[0] = (-319L);
      NumberUtils.min(longArray0);
      NumberUtils.toInt((String) null);
      double[] doubleArray0 = new double[2];
      doubleArray0[0] = (double) 0.0F;
      doubleArray0[1] = (double) 0.0F;
      NumberUtils.max(doubleArray0);
      NumberUtils.min(doubleArray0);
      try { 
        NumberUtils.createNumber("+g2wC");
        fail("Expecting exception: NumberFormatException");
      
      } catch(NumberFormatException e) {
         //
         // +g2wC is not a valid number.
         //
         verifyException("org.apache.commons.lang3.math.NumberUtils", e);
      }
  }
}
