/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:27:10 UTC 2020
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
      String[] stringArray0 = new String[8];
      stringArray0[0] = "ClassNotFoundException occurred during 1.6 backcompat code";
      stringArray0[1] = "r(5`#Ttq4";
      // Undeclared exception!
      StringUtils.replaceEach("r(5`#Ttq4", stringArray0, stringArray0);
  }
}
