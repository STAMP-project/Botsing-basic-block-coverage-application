/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 21:46:28 UTC 2021
 */

package org.mockito.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.mockito.internal.MockitoCore;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class MockitoCore_ESTest extends MockitoCore_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MockitoCore mockitoCore0 = new MockitoCore();
      Object object0 = new Object();
      // Undeclared exception!
      mockitoCore0.when(object0);
  }
}
