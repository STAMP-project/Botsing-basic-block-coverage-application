/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:16:22 UTC 2020
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(15);
      SumSincFunction sumSincFunction0 = new SumSincFunction(15);
      GoalType goalType0 = GoalType.MAXIMIZE;
      double[] doubleArray0 = new double[7];
      doubleArray0[0] = (double) 15;
      bOBYQAOptimizer0.getEvaluations();
      doubleArray0[1] = Double.POSITIVE_INFINITY;
      bOBYQAOptimizer0.getEvaluations();
      bOBYQAOptimizer0.getEvaluations();
      BOBYQAOptimizer bOBYQAOptimizer1 = new BOBYQAOptimizer(30);
      // Undeclared exception!
      bOBYQAOptimizer1.optimize(36, sumSincFunction0, goalType0, doubleArray0);
  }
}
