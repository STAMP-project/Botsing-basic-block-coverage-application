/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 15:56:50 UTC 2020
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
      RandomStringUtils.randomAlphanumeric(0);
      RandomStringUtils.random(2078);
      RandomStringUtils.randomAlphanumeric(0);
      RandomStringUtils randomStringUtils0 = new RandomStringUtils();
      RandomStringUtils.randomAlphanumeric(2078);
      RandomStringUtils.randomAlphabetic(0);
      RandomStringUtils.random(1, false, false);
      char[] charArray0 = new char[7];
      charArray0[0] = '`';
      charArray0[1] = ',';
      charArray0[2] = 'n';
      charArray0[3] = 'H';
      charArray0[4] = '\\';
      charArray0[5] = 'y';
      charArray0[6] = ' ';
      Random random0 = mock(Random.class, new ViolatedAssumptionAnswer());
      doReturn(0).when(random0).nextInt(anyInt());
      // Undeclared exception!
      RandomStringUtils.random(1, 2078, 0, false, false, charArray0, random0);
  }
}
