/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:12:04 UTC 2020
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
public class BOBYQAOptimizer_ESTest extends BOBYQAOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      int int0 = 6;
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(6);
      GoalType goalType0 = GoalType.MAXIMIZE;
      SumSincFunction sumSincFunction0 = new SumSincFunction((-2611.688197));
      sumSincFunction0.gradient();
      double[] doubleArray0 = new double[23];
      doubleArray0[0] = (-2611.688197);
      doubleArray0[15] = (-2.38);
      doubleArray0[2] = (-2.38);
      bOBYQAOptimizer0.getEvaluations();
      sumSincFunction0.value(doubleArray0);
      doubleArray0[22] = (double) (-934);
      doubleArray0[4] = (double) (-934);
      BOBYQAOptimizer bOBYQAOptimizer1 = new BOBYQAOptimizer(2412);
      GoalType goalType1 = GoalType.MAXIMIZE;
      SumSincFunction sumSincFunction1 = new SumSincFunction(3067.20733382094);
      sumSincFunction0.gradient();
      sumSincFunction1.value(doubleArray0);
      sumSincFunction1.partialDerivative(6);
      BOBYQAOptimizer bOBYQAOptimizer2 = new BOBYQAOptimizer(50);
      // Undeclared exception!
      bOBYQAOptimizer2.optimize(120, sumSincFunction0, goalType1, doubleArray0);
  }
}
