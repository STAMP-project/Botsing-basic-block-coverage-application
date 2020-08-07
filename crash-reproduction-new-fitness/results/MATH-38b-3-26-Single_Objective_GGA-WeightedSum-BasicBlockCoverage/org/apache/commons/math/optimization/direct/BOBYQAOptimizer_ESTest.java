/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:58:31 UTC 2020
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(20, 20, 20);
      SumSincFunction sumSincFunction0 = new SumSincFunction(20);
      MultivariateFunction multivariateFunction0 = sumSincFunction0.partialDerivative(2);
      GoalType goalType0 = GoalType.MAXIMIZE;
      double[] doubleArray0 = new double[6];
      doubleArray0[0] = (double) 20;
      doubleArray0[1] = 105.411630990037;
      doubleArray0[2] = (double) 2;
      doubleArray0[3] = (double) 2;
      doubleArray0[4] = (double) 20;
      doubleArray0[5] = 53.07755249;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(20, multivariateFunction0, goalType0, doubleArray0);
  }
}
