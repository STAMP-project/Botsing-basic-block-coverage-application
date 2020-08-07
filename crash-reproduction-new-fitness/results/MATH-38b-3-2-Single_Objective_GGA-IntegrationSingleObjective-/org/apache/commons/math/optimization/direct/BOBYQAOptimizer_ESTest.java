/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:08:31 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.MultivariateFunction;
import org.apache.commons.math.analysis.SumSincFunction;
import org.apache.commons.math.optimization.GoalType;
import org.apache.commons.math.optimization.direct.BOBYQAOptimizer;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BOBYQAOptimizer_ESTest extends BOBYQAOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(14);
      SumSincFunction sumSincFunction0 = new SumSincFunction(14);
      bOBYQAOptimizer0.getEvaluations();
      SumSincFunction sumSincFunction1 = new SumSincFunction(0);
      bOBYQAOptimizer0.getEvaluations();
      sumSincFunction0.gradient();
      MultivariateFunction multivariateFunction0 = sumSincFunction0.partialDerivative(0);
      sumSincFunction1.partialDerivative(3);
      double double0 = (-1023.3482698179165);
      GoalType goalType0 = GoalType.MAXIMIZE;
      double[] doubleArray0 = new double[4];
      doubleArray0[0] = (double) 22;
      doubleArray0[1] = (double) 14;
      doubleArray0[2] = (double) 14;
      doubleArray0[3] = (double) 3;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(34, multivariateFunction0, goalType0, doubleArray0);
  }
}
