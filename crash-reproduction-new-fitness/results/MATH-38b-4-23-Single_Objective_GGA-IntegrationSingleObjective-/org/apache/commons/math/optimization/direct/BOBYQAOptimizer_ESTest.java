/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:36:08 UTC 2020
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(20, 20, 20);
      SumSincFunction sumSincFunction0 = new SumSincFunction(20);
      double[] doubleArray0 = new double[5];
      doubleArray0[0] = 0.0;
      doubleArray0[1] = (double) 20;
      doubleArray0[2] = 0.0;
      doubleArray0[3] = 0.0;
      doubleArray0[4] = (double) 20;
      sumSincFunction0.value(doubleArray0);
      BOBYQAOptimizer bOBYQAOptimizer1 = new BOBYQAOptimizer(10);
      GoalType goalType0 = GoalType.MAXIMIZE;
      double[] doubleArray1 = new double[7];
      doubleArray1[0] = (-0.5);
      doubleArray1[1] = (double) (-15);
      doubleArray1[2] = (double) 2319;
      doubleArray1[3] = (double) 20;
      doubleArray1[4] = 0.0;
      doubleArray1[5] = (double) 20;
      doubleArray1[6] = (double) 20;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(2319, sumSincFunction0, goalType0, doubleArray1);
  }
}
