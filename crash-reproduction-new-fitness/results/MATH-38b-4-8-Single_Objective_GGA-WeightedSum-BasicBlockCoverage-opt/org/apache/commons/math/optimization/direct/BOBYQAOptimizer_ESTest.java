/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:59:29 UTC 2021
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
      int int0 = 21;
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(21);
      int int1 = 152;
      double double0 = 16.0;
      double[] doubleArray0 = new double[6];
      doubleArray0[0] = 16.0;
      doubleArray0[1] = 16.0;
      doubleArray0[2] = (double) 21;
      doubleArray0[3] = 16.0;
      doubleArray0[4] = (double) 152;
      doubleArray0[5] = 16.0;
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray1 = new double[7];
      SumSincFunction sumSincFunction0 = new SumSincFunction(16.0);
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(6035, sumSincFunction0, goalType0, doubleArray1);
  }
}
