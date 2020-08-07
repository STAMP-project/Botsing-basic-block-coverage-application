/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:07:08 UTC 2020
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
public class BaseAbstractMultivariateOptimizer_ESTest extends BaseAbstractMultivariateOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      GoalType goalType0 = GoalType.MAXIMIZE;
      int int0 = 64;
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(64, 64, 64);
      int int1 = (-4088);
      SumSincFunction sumSincFunction0 = new SumSincFunction((-4088));
      sumSincFunction0.partialDerivative(64);
      double[] doubleArray0 = new double[10];
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(64, sumSincFunction0, goalType0, doubleArray0);
  }
}
