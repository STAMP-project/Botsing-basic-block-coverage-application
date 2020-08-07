/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:51:32 UTC 2020
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
      byte byte0 = (byte)115;
      NumberUtils.min((byte)115, (byte) (-116), (byte)115);
      try { 
        NumberUtils.createNumber("#l{j");
        fail("Expecting exception: NumberFormatException");
      
      } catch(NumberFormatException e) {
         //
         // For input string: \"l{j\"
         //
         verifyException("java.lang.NumberFormatException", e);
      }
  }
}
