/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 23:29:31 UTC 2020
 */

package org.mockito.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.mockito.internal.MockitoCore;
import org.mockito.internal.creation.MockSettingsImpl;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class MockitoCore_ESTest extends MockitoCore_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MockitoCore mockitoCore0 = new MockitoCore();
      MockitoCore mockitoCore1 = new MockitoCore();
      Object[] objectArray0 = new Object[11];
      objectArray0[0] = (Object) mockitoCore0;
      objectArray0[1] = (Object) mockitoCore0;
      MockSettingsImpl mockSettingsImpl0 = new MockSettingsImpl();
      // Undeclared exception!
      mockitoCore0.when(mockSettingsImpl0);
  }
}
