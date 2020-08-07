/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 15:56:38 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Random;
import org.apache.commons.lang3.RandomStringUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class RandomStringUtils_ESTest extends RandomStringUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      RandomStringUtils.randomAscii(0);
      RandomStringUtils.randomAlphanumeric(0);
      RandomStringUtils randomStringUtils0 = new RandomStringUtils();
      int int0 = 3280;
      RandomStringUtils.random(0, 3280, 3280, true, true);
      char[] charArray0 = new char[1];
      charArray0[0] = 'S';
      RandomStringUtils.random(0, charArray0);
      int int1 = 1184;
      boolean boolean0 = true;
      Random random0 = mock(Random.class, new ViolatedAssumptionAnswer());
      doReturn(0).when(random0).nextInt(anyInt());
      // Undeclared exception!
      RandomStringUtils.random(56320, 1184, 56320, true, true, charArray0, random0);
  }
}
