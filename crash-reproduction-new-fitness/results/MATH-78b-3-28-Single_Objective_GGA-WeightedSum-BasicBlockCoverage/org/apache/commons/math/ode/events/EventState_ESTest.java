/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:45:13 UTC 2020
 */

package org.apache.commons.math.ode.events;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import org.apache.commons.math.ode.events.EventHandler;
import org.apache.commons.math.ode.events.EventState;
import org.apache.commons.math.ode.nonstiff.StepProblem;
import org.apache.commons.math.ode.sampling.DummyStepInterpolator;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EventState_ESTest extends EventState_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      EventHandler eventHandler0 = mock(EventHandler.class, new ViolatedAssumptionAnswer());
      doReturn((-1738.4944983080775), (-1560.6190000000001), 0.0, 0.0).when(eventHandler0).g(anyDouble() , any(double[].class));
      double[] doubleArray0 = new double[2];
      DummyStepInterpolator dummyStepInterpolator0 = new DummyStepInterpolator(doubleArray0, false);
      dummyStepInterpolator0.copy();
      StepProblem stepProblem0 = new StepProblem(194.7313967844634, 1709.0, (-1023.0));
      EventState eventState0 = new EventState(eventHandler0, (-2154.591463), 0.0, 3);
      DummyStepInterpolator dummyStepInterpolator1 = new DummyStepInterpolator(doubleArray0, false);
      dummyStepInterpolator0.copy();
      eventState0.evaluateStep(dummyStepInterpolator1);
      eventState0.getMaxCheckInterval();
      eventState0.evaluateStep(dummyStepInterpolator1);
  }
}
