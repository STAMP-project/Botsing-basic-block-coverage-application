/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:30:51 UTC 2021
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
      double double0 = 0.0;
      NumberUtils.toDouble("?5qRoifrSgE*BJPG[W", 0.0);
      // Undeclared exception!
      NumberUtils.createInteger("4G");
  }
}
