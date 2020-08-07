/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:58:39 UTC 2020
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
      String string0 = "";
      NumberUtils.toInt("");
      NumberUtils.max((short)0, (short)0, (short)0);
      NumberUtils.toInt("", (-6065));
      double[] doubleArray0 = new double[2];
      doubleArray0[0] = (double) (-6065);
      doubleArray0[1] = (double) (-6065);
      NumberUtils.max(doubleArray0);
      NumberUtils.min(doubleArray0);
      long[] longArray0 = new long[4];
      longArray0[0] = (long) (-6065);
      longArray0[1] = (long) (short)0;
      longArray0[2] = (long) (-6065);
      longArray0[3] = (long) (short)0;
      NumberUtils.max(longArray0);
      float[] floatArray0 = new float[1];
      floatArray0[0] = (float) (short)0;
      NumberUtils.max(floatArray0);
      try { 
        NumberUtils.createNumber("N(>tnyb8^n])w%wm");
        fail("Expecting exception: NumberFormatException");
      
      } catch(NumberFormatException e) {
         //
         // N(>tnyb8^n])w%wm is not a valid number.
         //
         verifyException("org.apache.commons.lang3.math.NumberUtils", e);
      }
  }
}
