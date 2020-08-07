/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:04:12 UTC 2020
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
import org.mockito.internal.invocation.Invocation;
import org.mockito.internal.invocation.MockitoMethod;
import org.mockito.internal.invocation.realmethod.RealMethod;
import org.mockito.internal.reporting.PrintSettings;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Invocation_ESTest extends Invocation_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Class<Invocation> class0 = Invocation.class;
      IsInstanceOf isInstanceOf0 = new IsInstanceOf(class0);
      MockitoMethod mockitoMethod0 = mock(MockitoMethod.class, new ViolatedAssumptionAnswer());
      doReturn(false).when(mockitoMethod0).isVarArgs();
      Object[] objectArray0 = new Object[1];
      objectArray0[0] = (Object) class0;
      Invocation invocation0 = new Invocation(isInstanceOf0, mockitoMethod0, objectArray0, 0, (RealMethod) null);
      invocation0.getArgumentsCount();
      invocation0.isVerified();
      invocation0.getArguments();
      invocation0.argumentsToMatchers();
      PrintSettings printSettings0 = mock(PrintSettings.class, new ViolatedAssumptionAnswer());
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
