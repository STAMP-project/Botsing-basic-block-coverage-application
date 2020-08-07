/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:05:15 UTC 2020
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(10);
      bOBYQAOptimizer0.getMaxEvaluations();
      GoalType goalType0 = GoalType.MINIMIZE;
      SumSincFunction sumSincFunction0 = new SumSincFunction(0);
      double[] doubleArray0 = new double[3];
      doubleArray0[0] = (-1121.20459795);
      doubleArray0[2] = (-1121.20459795);
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(10, sumSincFunction0, goalType0, doubleArray0);
  }
}
