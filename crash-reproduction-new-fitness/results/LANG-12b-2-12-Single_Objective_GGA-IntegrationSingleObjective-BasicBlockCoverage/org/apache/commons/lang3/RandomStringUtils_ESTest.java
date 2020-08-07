/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:32:45 UTC 2020
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
      RandomStringUtils.randomAlphabetic(123);
      char[] charArray0 = new char[5];
      charArray0[0] = '%';
      charArray0[1] = '9';
      charArray0[2] = '\\';
      charArray0[3] = '<';
      charArray0[4] = '#';
      RandomStringUtils.random(123, charArray0);
      RandomStringUtils.random(123, false, false);
      RandomStringUtils.randomAlphabetic(0);
      RandomStringUtils.random(0, charArray0);
      RandomStringUtils.random(0, "\u96C4\u52B5\u7A72\u2EFC\u62D0\uD4A9\u890B\uF2F4\u43F2\uC149\u357B\u711D\u5387\u1C16\u856A\uC6D7\u59EC\u2F37\u4E88\u327A\u88D8\u0C89\uC63C\uAD00\u6BDF\uC959\u9274\uD545\uED0A\u0AB7\u898E\u52F4\u6B9A\u1870\u6F6C\u7708\u02E3\u4CA0\u1D9D\u7FF4\u3F4D\u55E9\uA4AF\u8768\u9F6E\u624C\u1499\uBFB8\u5F4A\uE89A\uBA4D\u46E3\u2BB4\u668F\uC064\u7535\u7BC7\u0983\u40EB\u2D92\u6E85\uD7C5\uF652\u5559\u2A98\uD97C\uDC68\uD80E\uDE2E\u1897\u1AE9\u49CA\u9CF7\uE950\u0508\u2773\u7A43\u98B5\u75A1\uBBAB\u5D3F\u2FC6\uBC68\u8D97\u2D56\uA59B\uCCA4\u4658\u14AB\u7519\uD006\uE656\uB6BC\uE34D\u488E\uD225\u591A\u2C5B\u0F9F\u7D0A\u1C4D\u6D1B\uA02C\u1723\u82CD\uBF49\u3A03\u4529\uA423\u9E5E\u8C3F\u87DC\u8A26\u4B07\uB4D3\u9901\u5658\u8339\u8339\uA412\uED69\u84E8\u0C40");
      RandomStringUtils randomStringUtils0 = new RandomStringUtils();
      Random random0 = mock(Random.class, new ViolatedAssumptionAnswer());
      doReturn(0).when(random0).nextInt(anyInt());
      // Undeclared exception!
      RandomStringUtils.random(123, 123, 0, false, false, charArray0, random0);
  }
}
