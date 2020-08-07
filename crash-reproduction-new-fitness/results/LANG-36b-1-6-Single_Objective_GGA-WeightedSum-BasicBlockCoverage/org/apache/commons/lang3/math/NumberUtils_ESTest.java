/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:54:30 UTC 2020
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
      byte byte0 = (byte)0;
      NumberUtils.toByte("", (byte)0);
      float[] floatArray0 = new float[4];
      floatArray0[0] = 1.0F;
      floatArray0[1] = (float) (byte)0;
      floatArray0[2] = (float) (byte)0;
      floatArray0[3] = (float) (byte)0;
      NumberUtils.max(floatArray0);
      NumberUtils.createLong((String) null);
      NumberUtils.min(floatArray0);
      try { 
        NumberUtils.createNumber("C638k^Hi>J n");
        fail("Expecting exception: NumberFormatException");
      
      } catch(NumberFormatException e) {
         //
         // C638k^Hi>J n is not a valid number.
         //
         verifyException("org.apache.commons.lang3.math.NumberUtils", e);
      }
  }
}
