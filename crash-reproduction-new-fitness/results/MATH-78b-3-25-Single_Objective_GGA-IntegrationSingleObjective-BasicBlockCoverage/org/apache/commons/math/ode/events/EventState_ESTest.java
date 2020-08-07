/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 03:18:54 UTC 2020
 */

package org.apache.commons.math.ode.events;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.ode.events.EventHandler;
import org.apache.commons.math.ode.events.EventState;
import org.apache.commons.math.ode.nonstiff.StepProblem;
import org.apache.commons.math.ode.sampling.StepInterpolator;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EventState_ESTest extends EventState_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      EventHandler eventHandler0 = mock(EventHandler.class, new ViolatedAssumptionAnswer());
      doReturn(0.0, 0.0).when(eventHandler0).g(anyDouble() , any(double[].class));
      int int0 = (-1777);
      EventState eventState0 = new EventState(eventHandler0, (-1426.8), (-1426.8), (-1777));
      StepProblem stepProblem0 = new StepProblem((-1426.8), (-1426.8), (-1426.8));
      double double0 = 1.0;
      EventState eventState1 = new EventState(stepProblem0, (-277.412462), 1.0, (-1777));
      StepInterpolator stepInterpolator0 = mock(StepInterpolator.class, new ViolatedAssumptionAnswer());
      doReturn(0.0).when(stepInterpolator0).getCurrentTime();
      doReturn((Object) null, (Object) null, (Object) null).when(stepInterpolator0).getInterpolatedState();
      doReturn(false, false).when(stepInterpolator0).isForward();
      eventState1.stop();
      StepInterpolator stepInterpolator1 = mock(StepInterpolator.class, new ViolatedAssumptionAnswer());
      doReturn(0.0).when(stepInterpolator1).getCurrentTime();
      doReturn((Object) null, (Object) null, (Object) null).when(stepInterpolator1).getInterpolatedState();
      doReturn(false, false).when(stepInterpolator1).isForward();
      eventState1.evaluateStep(stepInterpolator1);
      StepInterpolator stepInterpolator2 = mock(StepInterpolator.class, new ViolatedAssumptionAnswer());
      doReturn(0.0).when(stepInterpolator2).getCurrentTime();
      doReturn((double[]) null).when(stepInterpolator2).getInterpolatedState();
      doReturn(false, false).when(stepInterpolator2).isForward();
      eventState0.reinitializeBegin(3439.6572867120567, (double[]) null);
      eventState0.evaluateStep(stepInterpolator2);
      eventState1.stepAccepted(1.0, (double[]) null);
      EventHandler eventHandler1 = mock(EventHandler.class, new ViolatedAssumptionAnswer());
      eventState1.stop();
      // Undeclared exception!
      eventState1.evaluateStep(stepInterpolator0);
  }
}
