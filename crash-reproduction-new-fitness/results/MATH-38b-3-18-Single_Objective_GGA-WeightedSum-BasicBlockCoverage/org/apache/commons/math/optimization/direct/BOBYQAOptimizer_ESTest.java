/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:54:00 UTC 2020
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(45);
      SumSincFunction sumSincFunction0 = new SumSincFunction(1737);
      GoalType goalType0 = GoalType.MAXIMIZE;
      double[] doubleArray0 = new double[8];
      doubleArray0[0] = Double.NaN;
      doubleArray0[1] = (double) 45;
      doubleArray0[2] = (double) 45;
      doubleArray0[3] = (double) 1737;
      doubleArray0[4] = (double) 45;
      doubleArray0[5] = (double) 1737;
      doubleArray0[6] = (double) 1737;
      doubleArray0[7] = (double) 45;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(1737, sumSincFunction0, goalType0, doubleArray0);
  }
}
