/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:28:39 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.lang3.StringUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class StringUtils_ESTest extends StringUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      StringUtils stringUtils0 = new StringUtils();
      StringUtils.substringsBetween("n=a.D_", "n=a.D_", (String) null);
      String[] stringArray0 = new String[6];
      stringArray0[0] = null;
      stringArray0[1] = "n=a.D_";
      stringArray0[2] = "n=a.D_";
      stringArray0[3] = "normalize";
      stringArray0[4] = "n=a.D_";
      stringArray0[5] = null;
      // Undeclared exception!
      StringUtils.replaceEach("n=a.D_", stringArray0, stringArray0);
  }
}
