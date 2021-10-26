/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:35:35 UTC 2021
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
      NumberUtils.toDouble("");
      NumberUtils.toFloat("");
      String string1 = "mE2=_";
      try { 
        NumberUtils.createNumber("mE2=_");
        fail("Expecting exception: NumberFormatException");
      
      } catch(NumberFormatException e) {
         //
         // mE2=_ is not a valid number.
         //
         verifyException("org.apache.commons.lang3.math.NumberUtils", e);
      }
  }
}
