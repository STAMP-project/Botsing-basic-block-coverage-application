/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:57:04 UTC 2020
 */

package org.apache.commons.math.ode.events;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.ode.events.EventHandler;
import org.apache.commons.math.ode.events.EventState;
import org.apache.commons.math.ode.nonstiff.StepProblem;
import org.apache.commons.math.ode.sampling.DummyStepInterpolator;
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
      doReturn(0.0).when(eventHandler0).g(anyDouble() , any(double[].class));
      StepInterpolator stepInterpolator0 = mock(StepInterpolator.class, new ViolatedAssumptionAnswer());
      doReturn(0.0).when(stepInterpolator0).getCurrentTime();
      doReturn((Object) null, (Object) null, (Object) null, (Object) null).when(stepInterpolator0).getInterpolatedState();
      doReturn(false, false).when(stepInterpolator0).isForward();
      double[] doubleArray0 = new double[1];
      StepProblem stepProblem0 = new StepProblem((-1153.9543), (-2596.4385708775153), 1.0);
      stepProblem0.eventOccurred((-2596.4385708775153), doubleArray0, true);
      EventState eventState0 = new EventState(stepProblem0, (-671.60193), (-2304.561918169516), 100);
      eventState0.reinitializeBegin(20, doubleArray0);
      eventState0.evaluateStep(stepInterpolator0);
      stepProblem0.eventOccurred((-883.7042828527), doubleArray0, false);
      EventState eventState1 = new EventState(eventHandler0, (-639.0142050886), 1024.0, 2);
      eventState1.reinitializeBegin(2305.0, doubleArray0);
      eventState0.stepAccepted(2616.4385708775153, (double[]) null);
      DummyStepInterpolator dummyStepInterpolator0 = new DummyStepInterpolator(doubleArray0, true);
      eventState0.stop();
      dummyStepInterpolator0.storeTime(100);
      eventState0.stop();
      eventState1.getEventTime();
      // Undeclared exception!
      eventState0.evaluateStep(dummyStepInterpolator0);
  }
}
