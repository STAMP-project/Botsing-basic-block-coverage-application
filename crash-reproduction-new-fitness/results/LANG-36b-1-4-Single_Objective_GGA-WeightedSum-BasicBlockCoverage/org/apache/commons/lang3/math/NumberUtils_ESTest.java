/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:54:26 UTC 2020
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
      double[] doubleArray0 = new double[4];
      doubleArray0[0] = 0.0;
      doubleArray0[1] = 530.28357866;
      doubleArray0[2] = 1.0;
      doubleArray0[3] = 0.0;
      NumberUtils.min(doubleArray0);
      long[] longArray0 = new long[3];
      longArray0[0] = (-634L);
      longArray0[1] = 0L;
      longArray0[2] = (-112L);
      NumberUtils.max(longArray0);
      NumberUtils.toDouble("s+#v~$h", 0.0);
      NumberUtils.max(doubleArray0);
      NumberUtils.toInt("dqIRc");
      NumberUtils.toByte("s+#v~$h");
      try { 
        NumberUtils.createNumber("dqIRc");
        fail("Expecting exception: NumberFormatException");
      
      } catch(NumberFormatException e) {
         //
         // dqIRc is not a valid number.
         //
         verifyException("org.apache.commons.lang3.math.NumberUtils", e);
      }
  }
}
