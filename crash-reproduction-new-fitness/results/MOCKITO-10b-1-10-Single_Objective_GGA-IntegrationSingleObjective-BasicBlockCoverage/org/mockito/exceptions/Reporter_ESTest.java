/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 23:19:35 UTC 2020
 */

package org.mockito.exceptions;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.mockito.exceptions.Reporter;
import org.mockito.internal.invocation.InvocationImpl;
import org.mockito.invocation.DescribedInvocation;
import org.mockito.invocation.Invocation;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.invocation.Location;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Reporter_ESTest extends Reporter_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      InvocationOnMock invocationOnMock0 = mock(InvocationOnMock.class, new ViolatedAssumptionAnswer());
      org.mockito.internal.reporting.Discrepancy discrepancy0 = mock(org.mockito.internal.reporting.Discrepancy.class, new ViolatedAssumptionAnswer());
      DescribedInvocation describedInvocation0 = mock(DescribedInvocation.class, new ViolatedAssumptionAnswer());
      Location location0 = mock(Location.class, new ViolatedAssumptionAnswer());
      Invocation invocation0 = mock(Invocation.class, new ViolatedAssumptionAnswer());
      DescribedInvocation describedInvocation1 = mock(DescribedInvocation.class, new ViolatedAssumptionAnswer());
      DescribedInvocation describedInvocation2 = mock(DescribedInvocation.class, new ViolatedAssumptionAnswer());
      Location location1 = mock(Location.class, new ViolatedAssumptionAnswer());
      DescribedInvocation describedInvocation3 = mock(DescribedInvocation.class, new ViolatedAssumptionAnswer());
      Location location2 = mock(Location.class, new ViolatedAssumptionAnswer());
      Invocation invocation1 = mock(Invocation.class, new ViolatedAssumptionAnswer());
      Exception exception0 = mock(Exception.class, new ViolatedAssumptionAnswer());
      DescribedInvocation describedInvocation4 = mock(DescribedInvocation.class, new ViolatedAssumptionAnswer());
      Location location3 = mock(Location.class, new ViolatedAssumptionAnswer());
      Reporter reporter0 = new Reporter();
      Class<InvocationImpl> class0 = InvocationImpl.class;
      // Undeclared exception!
      reporter0.serializableWontWorkForObjectsThatDontImplementSerializable(class0);
  }
}
