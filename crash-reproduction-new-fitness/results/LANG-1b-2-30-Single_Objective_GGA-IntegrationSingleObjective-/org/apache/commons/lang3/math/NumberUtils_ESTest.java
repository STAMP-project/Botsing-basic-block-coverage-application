/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:51:51 UTC 2020
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
      NumberUtils.min(1484.0F, 1484.0F, 1484.0F);
      double[] doubleArray0 = new double[4];
      doubleArray0[0] = (double) 1484.0F;
      doubleArray0[2] = (double) 1484.0F;
      doubleArray0[3] = (double) 1484.0F;
      String string0 = null;
      NumberUtils.createNumber((String) null);
      NumberUtils.toLong("n+ jE(|qJ?9=S`JX7", 1098L);
      NumberUtils.toShort("I1Opl/jlqj>5u! c%");
      NumberUtils.min(2867L, 1098L, (-1L));
      NumberUtils.isDigits("0x%6B`R})/6{.,");
      try { 
        NumberUtils.createNumber("0x");
        fail("Expecting exception: NumberFormatException");
      
      } catch(NumberFormatException e) {
         //
         // For input string: \"\"
         //
         verifyException("java.lang.NumberFormatException", e);
      }
  }
}
