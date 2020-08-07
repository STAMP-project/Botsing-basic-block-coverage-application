/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:54:14 UTC 2020
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
      NumberUtils.toDouble("7KL,$T()Z=0Elj", 340.867);
      NumberUtils.max(0L, 0L, 0L);
      double[] doubleArray0 = new double[7];
      doubleArray0[0] = (double) 0L;
      doubleArray0[1] = (double) 0L;
      doubleArray0[2] = (double) 0L;
      doubleArray0[3] = (double) 0L;
      doubleArray0[4] = 340.867;
      doubleArray0[5] = (double) 0L;
      doubleArray0[6] = (double) 0L;
      NumberUtils.min(doubleArray0);
      NumberUtils.max(doubleArray0);
      short short0 = (short)0;
      NumberUtils.toShort("", (short)0);
      NumberUtils.max(0L, (long) (short)0, 2443L);
      String string0 = "7BmI0#=?K^15N\\XA51S";
      try { 
        NumberUtils.createNumber("7BmI0#=?K^15NXA51S");
        fail("Expecting exception: NumberFormatException");
      
      } catch(NumberFormatException e) {
         //
         // 7BmI0#=?K^15NXA51S is not a valid number.
         //
         verifyException("org.apache.commons.lang3.math.NumberUtils", e);
      }
  }
}
