/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:29:39 UTC 2020
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
      StringUtils.containsNone("BJ{ET", "");
      StringUtils.stripToNull("");
      StringUtils.isNumericSpace((String) null);
      char[] charArray0 = new char[2];
      charArray0[0] = '-';
      charArray0[1] = '!';
      StringUtils.indexOfAny("org.apache.commons.lang3.StringUtils", charArray0);
      StringUtils.isNumericSpace("");
      String[] stringArray0 = new String[9];
      stringArray0[0] = "org.apache.commons.lang3.StringUtils";
      stringArray0[1] = "BJ{ET";
      stringArray0[2] = "org.apache.commons.lang3.StringUtils";
      stringArray0[3] = null;
      stringArray0[4] = "";
      stringArray0[5] = "";
      stringArray0[6] = "org.apache.commons.lang3.StringUtils";
      stringArray0[7] = "";
      stringArray0[8] = "BJ{ET";
      // Undeclared exception!
      StringUtils.replaceEach("org.apache.commons.lang3.StringUtils", stringArray0, stringArray0);
  }
}
