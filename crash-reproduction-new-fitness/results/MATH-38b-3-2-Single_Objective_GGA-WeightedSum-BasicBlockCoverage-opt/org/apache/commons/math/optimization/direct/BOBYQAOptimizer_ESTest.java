/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:57:21 UTC 2021
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(30);
      SumSincFunction sumSincFunction0 = new SumSincFunction(30);
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray0 = new double[8];
      doubleArray0[0] = (double) 30;
      doubleArray0[1] = (double) 30;
      doubleArray0[2] = (double) 30;
      doubleArray0[3] = (double) 30;
      doubleArray0[4] = 0.0;
      doubleArray0[5] = (double) 30;
      doubleArray0[6] = (double) 30;
      doubleArray0[7] = (double) 30;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(30, sumSincFunction0, goalType0, doubleArray0);
  }
}
