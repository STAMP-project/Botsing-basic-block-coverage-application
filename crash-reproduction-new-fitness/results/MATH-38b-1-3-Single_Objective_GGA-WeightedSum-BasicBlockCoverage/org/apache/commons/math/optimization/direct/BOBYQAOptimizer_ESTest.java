/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:27:54 UTC 2020
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(28);
      SumSincFunction sumSincFunction0 = new SumSincFunction(28);
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray0 = new double[8];
      doubleArray0[0] = (double) 28;
      doubleArray0[1] = (double) 28;
      doubleArray0[2] = (double) 28;
      doubleArray0[3] = 1330.533030192429;
      doubleArray0[4] = (double) 28;
      doubleArray0[5] = (double) 28;
      doubleArray0[6] = 1330.533030192429;
      doubleArray0[7] = (double) 28;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(28, sumSincFunction0, goalType0, doubleArray0);
  }
}
