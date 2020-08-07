/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:07:51 UTC 2020
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
      double double0 = (-810.417712);
      int int0 = 10;
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(10);
      int int1 = 1694;
      SumSincFunction sumSincFunction0 = new SumSincFunction(10);
      GoalType goalType0 = GoalType.MINIMIZE;
      SumSincFunction sumSincFunction1 = new SumSincFunction(1694);
      double[] doubleArray0 = new double[4];
      doubleArray0[0] = (double) 1694;
      doubleArray0[1] = (double) 10;
      doubleArray0[2] = (double) 10;
      doubleArray0[3] = (-810.417712);
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(20, sumSincFunction1, goalType0, doubleArray0);
  }
}
