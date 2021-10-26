/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:57:08 UTC 2021
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SumSincFunction;
import org.apache.commons.math.optimization.GoalType;
import org.apache.commons.math.optimization.direct.BOBYQAOptimizer;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractMultivariateSimpleBoundsOptimizer_ESTest extends BaseAbstractMultivariateSimpleBoundsOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      SumSincFunction sumSincFunction0 = new SumSincFunction((-16.0));
      double[] doubleArray0 = new double[9];
      SumSincFunction sumSincFunction1 = new SumSincFunction(23);
      sumSincFunction0.gradient();
      sumSincFunction0.gradient();
      sumSincFunction0.gradient();
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(23, 23, 23);
      sumSincFunction0.partialDerivative(23);
      sumSincFunction0.partialDerivative(15);
      bOBYQAOptimizer0.getMaxEvaluations();
      sumSincFunction1.value(doubleArray0);
      sumSincFunction0.partialDerivative(2240);
      GoalType goalType0 = GoalType.MINIMIZE;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(23, sumSincFunction1, goalType0, doubleArray0);
  }
}
