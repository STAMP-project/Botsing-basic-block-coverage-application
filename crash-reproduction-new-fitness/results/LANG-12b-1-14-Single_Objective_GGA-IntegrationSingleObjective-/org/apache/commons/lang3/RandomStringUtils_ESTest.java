/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:34:54 UTC 2020
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
      RandomStringUtils randomStringUtils0 = new RandomStringUtils();
      int int0 = 1935;
      int int1 = 55296;
      boolean boolean0 = true;
      char[] charArray0 = new char[2];
      charArray0[0] = 'm';
      charArray0[1] = 'I';
      Random random0 = mock(Random.class, new ViolatedAssumptionAnswer());
      doReturn(0).when(random0).nextInt(anyInt());
      // Undeclared exception!
      RandomStringUtils.random(1935, 1935, 55296, true, true, charArray0, random0);
  }
}
