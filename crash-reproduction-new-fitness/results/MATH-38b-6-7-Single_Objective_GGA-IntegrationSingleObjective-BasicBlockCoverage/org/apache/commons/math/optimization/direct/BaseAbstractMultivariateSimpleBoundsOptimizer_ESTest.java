/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:09:26 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.MultivariateFunction;
import org.apache.commons.math.analysis.SumSincFunction;
import org.apache.commons.math.optimization.GoalType;
import org.apache.commons.math.optimization.direct.BOBYQAOptimizer;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractMultivariateSimpleBoundsOptimizer_ESTest extends BaseAbstractMultivariateSimpleBoundsOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MultivariateFunction multivariateFunction0 = mock(MultivariateFunction.class, new ViolatedAssumptionAnswer());
      int int0 = (-1181);
      int int1 = 19;
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(19);
      GoalType goalType0 = GoalType.MAXIMIZE;
      int int2 = 30;
      GoalType goalType1 = GoalType.MINIMIZE;
      double[] doubleArray0 = new double[5];
      doubleArray0[0] = 349.47688712290176;
      double double0 = 244.072808;
      SumSincFunction sumSincFunction0 = new SumSincFunction(552.81114);
      sumSincFunction0.gradient();
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(120, sumSincFunction0, goalType0, doubleArray0);
  }
}
