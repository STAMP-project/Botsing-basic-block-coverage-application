/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 03:15:16 UTC 2020
 */

package org.mockito.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.lang.reflect.Array;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.mockito.internal.MockitoCore;
import org.mockito.internal.creation.MockSettingsImpl;
import org.mockito.internal.progress.MockingProgressImpl;
import org.mockito.internal.verification.Only;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class MockitoCore_ESTest extends MockitoCore_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MockingProgressImpl mockingProgressImpl0 = new MockingProgressImpl();
      Class<Integer> class0 = Integer.TYPE;
      Class<Object>[] classArray0 = (Class<Object>[]) Array.newInstance(Class.class, 1);
      Only only0 = new Only();
      MockSettingsImpl mockSettingsImpl0 = new MockSettingsImpl();
      MockitoCore mockitoCore0 = new MockitoCore();
      Integer integer0 = new Integer(9);
      // Undeclared exception!
      mockitoCore0.when(integer0);
  }
}
