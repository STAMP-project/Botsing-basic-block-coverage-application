/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:09:14 UTC 2020
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
      stringArray0[0] = "JcTrcd@iYN*H";
      stringArray0[1] = "JcTrcd@iYN*H";
      stringArray0[0] = "JcTrcd@iYN*H";
      stringArray0[3] = "JcTrcd@iYN*H";
      stringArray0[4] = "JcTrcd@iYN*H";
      stringArray0[5] = "JcTrcd@iYN*H";
      // Undeclared exception!
      StringUtils.replaceEach("JcTrcd@iYN*H", stringArray0, stringArray0);
  }
}
