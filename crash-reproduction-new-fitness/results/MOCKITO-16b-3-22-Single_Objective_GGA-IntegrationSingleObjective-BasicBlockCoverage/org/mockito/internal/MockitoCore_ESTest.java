/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 03:53:18 UTC 2020
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
import org.mockito.internal.progress.ThreadSafeMockingProgress;
import org.mockito.internal.stubbing.InvocationContainerImpl;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class MockitoCore_ESTest extends MockitoCore_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MockitoCore mockitoCore0 = new MockitoCore();
      MockSettingsImpl mockSettingsImpl0 = new MockSettingsImpl();
      ThreadSafeMockingProgress threadSafeMockingProgress0 = new ThreadSafeMockingProgress();
      InvocationContainerImpl invocationContainerImpl0 = new InvocationContainerImpl(threadSafeMockingProgress0);
      // Undeclared exception!
      mockitoCore0.when((Object) invocationContainerImpl0);
  }
}
