/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 23:00:26 UTC 2020
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
      StepProblem stepProblem0 = new StepProblem((-1123.2521738986352), (-1123.2521738986352), (-1123.2521738986352));
      double[] doubleArray0 = new double[2];
      DummyStepInterpolator dummyStepInterpolator0 = new DummyStepInterpolator(doubleArray0, true);
      StepProblem stepProblem1 = new StepProblem((-1123.2521738986352), 4546.545261288995, (-1123.2521738986352));
      EventState eventState0 = new EventState(stepProblem1, 4546.545261288995, 2165, 2165);
      StepInterpolator stepInterpolator0 = dummyStepInterpolator0.copy();
      stepProblem1.setRate(2165);
      eventState0.evaluateStep(stepInterpolator0);
      eventState0.evaluateStep(dummyStepInterpolator0);
      eventState0.stepAccepted(4546.545261288995, doubleArray0);
      dummyStepInterpolator0.storeTime((-1.499999));
      // Undeclared exception!
      eventState0.evaluateStep(dummyStepInterpolator0);
  }
}
