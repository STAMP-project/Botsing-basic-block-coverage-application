/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:36:58 UTC 2020
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
      int int0 = 20;
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(20, 20, 20);
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray0 = new double[5];
      SumSincFunction sumSincFunction0 = new SumSincFunction(2370.90740615);
      MultivariateFunction multivariateFunction1 = sumSincFunction0.partialDerivative(2);
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(20, multivariateFunction1, goalType0, doubleArray0);
  }
}
