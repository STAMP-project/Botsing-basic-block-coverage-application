/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:03:53 UTC 2020
 */

package org.mockito.internal.invocation;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.mockito.internal.invocation.Invocation;
import org.mockito.internal.invocation.MockitoMethod;
import org.mockito.internal.invocation.realmethod.RealMethod;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Invocation_ESTest extends Invocation_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Object object0 = new Object();
      MockitoMethod mockitoMethod0 = mock(MockitoMethod.class, new ViolatedAssumptionAnswer());
      doReturn(false).when(mockitoMethod0).isVarArgs();
      Object[] objectArray0 = new Object[3];
      objectArray0[0] = (Object) mockitoMethod0;
      Invocation invocation0 = new Invocation(object0, mockitoMethod0, objectArray0, (-2469), (RealMethod) null);
      try { 
        invocation0.callRealMethod();
        fail("Expecting exception: NullPointerException");
      
      } catch(NullPointerException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("org.mockito.internal.invocation.Invocation", e);
      }
  }
}
