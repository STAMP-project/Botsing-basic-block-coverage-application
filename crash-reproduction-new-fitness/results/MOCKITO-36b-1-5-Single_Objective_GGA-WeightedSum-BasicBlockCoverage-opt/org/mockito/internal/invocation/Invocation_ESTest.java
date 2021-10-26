/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 15:22:19 UTC 2021
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
import org.mockito.cglib.proxy.MethodProxy;
import org.mockito.internal.creation.DelegatingMockitoMethodProxy;
import org.mockito.internal.invocation.Invocation;
import org.mockito.internal.invocation.MockitoMethod;
import org.mockito.internal.invocation.realmethod.FilteredCGLIBProxyRealMethod;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Invocation_ESTest extends Invocation_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Object object0 = new Object();
      MockitoMethod mockitoMethod0 = mock(MockitoMethod.class, new ViolatedAssumptionAnswer());
      doReturn(false).when(mockitoMethod0).isVarArgs();
      String string0 = "o";
      Object[] objectArray0 = new Object[5];
      objectArray0[0] = (Object) mockitoMethod0;
      objectArray0[1] = (Object) mockitoMethod0;
      objectArray0[2] = (Object) "o";
      objectArray0[3] = (Object) "o";
      objectArray0[4] = object0;
      DelegatingMockitoMethodProxy delegatingMockitoMethodProxy0 = new DelegatingMockitoMethodProxy((MethodProxy) null);
      FilteredCGLIBProxyRealMethod filteredCGLIBProxyRealMethod0 = new FilteredCGLIBProxyRealMethod(delegatingMockitoMethodProxy0);
      Invocation invocation0 = new Invocation("o", mockitoMethod0, objectArray0, (-1), filteredCGLIBProxyRealMethod0);
      try { 
        invocation0.callRealMethod();
        fail("Expecting exception: NullPointerException");
      
      } catch(NullPointerException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("org.mockito.internal.creation.AbstractMockitoMethodProxy", e);
      }
  }
}
