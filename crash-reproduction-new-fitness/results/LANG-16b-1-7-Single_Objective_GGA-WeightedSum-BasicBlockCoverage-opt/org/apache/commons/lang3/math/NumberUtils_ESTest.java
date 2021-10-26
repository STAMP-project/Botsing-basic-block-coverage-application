/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:27:16 UTC 2021
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
      NumberUtils numberUtils0 = new NumberUtils();
      String string0 = "<";
      NumberUtils.toShort("<");
      int[] intArray0 = new int[7];
      intArray0[0] = (int) (short)0;
      intArray0[1] = (int) (short)0;
      intArray0[2] = 88;
      intArray0[3] = (int) (short)0;
      intArray0[4] = (int) (short)0;
      intArray0[5] = (int) (short)0;
      intArray0[6] = (int) (short)0;
      NumberUtils.min(intArray0);
      NumberUtils.toShort("<");
      NumberUtils.min((byte)9, (byte)9, (byte)9);
      NumberUtils.min(Float.NaN, Float.NaN, (float) 0);
      NumberUtils.min(0.0F, (float) 0, 1042.785F);
      try { 
        NumberUtils.createNumber("<");
        fail("Expecting exception: NumberFormatException");
      
      } catch(NumberFormatException e) {
         //
         // < is not a valid number.
         //
         verifyException("org.apache.commons.lang3.math.NumberUtils", e);
      }
  }
}
