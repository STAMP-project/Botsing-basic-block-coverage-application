/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:12:15 UTC 2020
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer((-2352));
      BOBYQAOptimizer bOBYQAOptimizer1 = new BOBYQAOptimizer(4);
      SumSincFunction sumSincFunction0 = new SumSincFunction(Double.NaN);
      MultivariateFunction multivariateFunction0 = sumSincFunction0.partialDerivative(4);
      GoalType goalType0 = GoalType.MAXIMIZE;
      BOBYQAOptimizer bOBYQAOptimizer2 = new BOBYQAOptimizer(16);
      SumSincFunction sumSincFunction1 = new SumSincFunction(487.151981369752);
      SumSincFunction sumSincFunction2 = new SumSincFunction(0.0);
      double[] doubleArray0 = new double[6];
      doubleArray0[0] = 0.0;
      doubleArray0[1] = 487.151981369752;
      doubleArray0[2] = (double) 275;
      doubleArray0[3] = (double) (-2352);
      doubleArray0[4] = (double) 275;
      doubleArray0[5] = 0.0;
      // Undeclared exception!
      bOBYQAOptimizer2.optimize(1281, multivariateFunction0, goalType0, doubleArray0);
  }
}
