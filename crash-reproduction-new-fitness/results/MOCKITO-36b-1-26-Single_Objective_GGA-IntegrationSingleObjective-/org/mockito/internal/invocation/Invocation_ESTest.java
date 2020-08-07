/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:03:58 UTC 2020
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
      doReturn(false).when(mockitoMethod0).isVarArgs();
      Integer integer0 = new Integer(25165824);
      Object[] objectArray0 = new Object[2];
      objectArray0[0] = (Object) isInstanceOf0;
      objectArray0[1] = (Object) mockitoMethod0;
      DelegatingMockitoMethodProxy delegatingMockitoMethodProxy0 = new DelegatingMockitoMethodProxy((MethodProxy) null);
      CGLIBProxyRealMethod cGLIBProxyRealMethod0 = new CGLIBProxyRealMethod(delegatingMockitoMethodProxy0);
      Invocation invocation0 = new Invocation(integer0, mockitoMethod0, objectArray0, (-1005), cGLIBProxyRealMethod0);
      invocation0.getSequenceNumber();
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
