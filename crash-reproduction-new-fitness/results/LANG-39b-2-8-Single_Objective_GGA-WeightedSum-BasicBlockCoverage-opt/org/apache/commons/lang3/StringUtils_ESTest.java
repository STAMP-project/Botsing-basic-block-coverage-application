/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:18:46 UTC 2021
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
      String[] stringArray0 = new String[9];
      stringArray0[0] = "...";
      stringArray0[1] = "";
      stringArray0[2] = "...";
      stringArray0[3] = "...";
      stringArray0[2] = "...";
      stringArray0[5] = "...";
      stringArray0[6] = "...";
      stringArray0[7] = "";
      stringArray0[8] = "...";
      // Undeclared exception!
      StringUtils.replaceEach("...", stringArray0, stringArray0);
  }
}
