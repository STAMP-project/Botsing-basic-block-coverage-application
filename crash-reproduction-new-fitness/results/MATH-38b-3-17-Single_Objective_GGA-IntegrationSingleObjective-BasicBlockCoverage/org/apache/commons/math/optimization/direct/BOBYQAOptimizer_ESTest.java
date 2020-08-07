/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:28:16 UTC 2020
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(8, 144, 43.2536219);
      SumSincFunction sumSincFunction0 = new SumSincFunction(43.2536219);
      double[] doubleArray0 = new double[3];
      doubleArray0[0] = 2139.96837228977;
      doubleArray0[1] = (double) (-3414);
      BOBYQAOptimizer bOBYQAOptimizer1 = new BOBYQAOptimizer(144, 8, 2139.96837228977);
      SumSincFunction sumSincFunction1 = new SumSincFunction(8);
      bOBYQAOptimizer0.getMaxEvaluations();
      GoalType goalType0 = GoalType.MINIMIZE;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(144, sumSincFunction1, goalType0, doubleArray0);
  }
}
