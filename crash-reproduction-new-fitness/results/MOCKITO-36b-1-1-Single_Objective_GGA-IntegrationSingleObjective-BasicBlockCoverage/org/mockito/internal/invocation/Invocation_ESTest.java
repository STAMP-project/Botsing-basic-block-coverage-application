/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:38:18 UTC 2020
 */

package org.mockito.internal.invocation;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.hamcrest.core.IsInstanceOf;
import org.junit.runner.RunWith;
import org.mockito.cglib.proxy.MethodProxy;
import org.mockito.internal.creation.DelegatingMockitoMethodProxy;
import org.mockito.internal.invocation.Invocation;
import org.mockito.internal.invocation.MockitoMethod;
import org.mockito.internal.invocation.realmethod.CGLIBProxyRealMethod;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Invocation_ESTest extends Invocation_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Class<Object> class0 = Object.class;
      IsInstanceOf isInstanceOf0 = new IsInstanceOf(class0);
      MockitoMethod mockitoMethod0 = mock(MockitoMethod.class, new ViolatedAssumptionAnswer());
      doReturn((String) null).when(mockitoMethod0).getName();
      doReturn(false).when(mockitoMethod0).isVarArgs();
      Object[] objectArray0 = null;
      Object[] objectArray1 = new Object[5];
      objectArray1[0] = (Object) class0;
      objectArray1[1] = (Object) mockitoMethod0;
      objectArray1[2] = (Object) mockitoMethod0;
      objectArray1[3] = (Object) class0;
      objectArray1[4] = (Object) mockitoMethod0;
      DelegatingMockitoMethodProxy delegatingMockitoMethodProxy0 = new DelegatingMockitoMethodProxy((MethodProxy) null);
      CGLIBProxyRealMethod cGLIBProxyRealMethod0 = new CGLIBProxyRealMethod(delegatingMockitoMethodProxy0);
      Invocation invocation0 = new Invocation(isInstanceOf0, mockitoMethod0, objectArray1, 10, cGLIBProxyRealMethod0);
      invocation0.getMock();
      invocation0.getMethodName();
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
