/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:28:41 UTC 2020
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
      String[] stringArray0 = new String[6];
      stringArray0[0] = "Cannot pad a negative amount: ";
      stringArray0[0] = "Cannot pad a negative amount: ";
      stringArray0[1] = "Cannot pad a negative amount: ";
      stringArray0[3] = "NFD";
      // Undeclared exception!
      StringUtils.replaceEach("Cannot pad a negative amount: ", stringArray0, stringArray0);
  }
}
