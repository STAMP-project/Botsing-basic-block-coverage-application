/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 03:53:11 UTC 2020
 */

package org.mockito.exceptions;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.mockito.exceptions.Reporter;
import org.mockito.internal.invocation.InvocationImpl;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Reporter_ESTest extends Reporter_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Reporter reporter0 = new Reporter();
      Class<InvocationImpl> class0 = InvocationImpl.class;
      // Undeclared exception!
      reporter0.serializableWontWorkForObjectsThatDontImplementSerializable(class0);
  }
}
