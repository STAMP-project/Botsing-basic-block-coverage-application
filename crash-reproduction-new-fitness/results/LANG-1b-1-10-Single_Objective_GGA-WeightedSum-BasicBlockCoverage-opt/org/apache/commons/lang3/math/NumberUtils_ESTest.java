/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:31:18 UTC 2021
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
      int[] intArray0 = new int[4];
      intArray0[0] = (-1);
      intArray0[1] = 1483;
      intArray0[2] = (-66);
      intArray0[3] = (-603);
      NumberUtils.min(intArray0);
      NumberUtils.isNumber("#");
      // Undeclared exception!
      NumberUtils.createInteger("#");
  }
}
