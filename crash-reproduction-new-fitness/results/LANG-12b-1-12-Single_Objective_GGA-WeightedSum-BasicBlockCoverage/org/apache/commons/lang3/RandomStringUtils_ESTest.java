/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 11:30:11 UTC 2020
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
      RandomStringUtils.random(1412, 1412, 1532, true, true);
      RandomStringUtils.randomAlphanumeric(0);
      char[] charArray0 = new char[6];
      charArray0[0] = 'b';
      charArray0[1] = '.';
      charArray0[2] = 'N';
      charArray0[3] = '>';
      charArray0[4] = 'R';
      charArray0[5] = 'z';
      Random random0 = mock(Random.class, new ViolatedAssumptionAnswer());
      doReturn(0).when(random0).nextInt(anyInt());
      // Undeclared exception!
      RandomStringUtils.random(1412, 1412, 1532, true, true, charArray0, random0);
  }
}
