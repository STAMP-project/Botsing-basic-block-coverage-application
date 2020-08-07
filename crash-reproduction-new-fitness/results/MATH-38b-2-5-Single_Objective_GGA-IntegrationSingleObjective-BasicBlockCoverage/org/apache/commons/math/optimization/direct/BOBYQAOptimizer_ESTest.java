/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:07:03 UTC 2020
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
      int int0 = 134;
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray0 = new double[4];
      SumSincFunction sumSincFunction0 = new SumSincFunction(134);
      sumSincFunction0.partialDerivative((-1006));
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(13, (-494.66601573582), 0.0);
      GoalType goalType1 = GoalType.MINIMIZE;
      BOBYQAOptimizer bOBYQAOptimizer1 = new BOBYQAOptimizer(13);
      int int1 = 301;
      // Undeclared exception!
      bOBYQAOptimizer1.optimize(301, sumSincFunction0, goalType1, doubleArray0);
  }
}
