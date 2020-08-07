/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:32:31 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.lang3.RandomStringUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class RandomStringUtils_ESTest extends RandomStringUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      RandomStringUtils.random(0, "");
      char[] charArray0 = new char[0];
      RandomStringUtils.random(0, charArray0);
      RandomStringUtils.randomAlphabetic(0);
      RandomStringUtils.random(0, 0, (-254), false, false, charArray0);
      RandomStringUtils.random(0, (-254), (-3897), true, false, charArray0);
      RandomStringUtils.randomAlphabetic(13);
      RandomStringUtils.randomAlphanumeric(1);
      RandomStringUtils.randomAscii(13);
      // Undeclared exception!
      RandomStringUtils.random(108, charArray0);
  }
}
