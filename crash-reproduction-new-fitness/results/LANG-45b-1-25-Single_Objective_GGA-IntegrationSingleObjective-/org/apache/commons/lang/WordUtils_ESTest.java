/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:59:24 UTC 2020
 */

package org.apache.commons.lang;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.lang.WordUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class WordUtils_ESTest extends WordUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      String string0 = " ";
      WordUtils.capitalizeFully(" ");
      String string1 = "AIX";
      int int0 = 2074;
      // Undeclared exception!
      WordUtils.abbreviate("AIX", 2074, 3, " ");
  }
}
