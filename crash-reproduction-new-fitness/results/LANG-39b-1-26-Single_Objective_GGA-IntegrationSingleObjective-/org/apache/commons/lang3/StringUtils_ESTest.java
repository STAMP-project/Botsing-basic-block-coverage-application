/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:33:29 UTC 2020
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
      StringUtils.stripEnd(",d#B')5VanN5/", ",d#B')5VanN5/");
      String[] stringArray0 = new String[5];
      stringArray0[0] = "~`_~GbAbX";
      String string0 = "";
      stringArray0[1] = "";
      stringArray0[3] = "~`_~GbAbX";
      stringArray0[4] = "";
      // Undeclared exception!
      StringUtils.replaceEach("~`_~GbAbX", stringArray0, stringArray0);
  }
}
