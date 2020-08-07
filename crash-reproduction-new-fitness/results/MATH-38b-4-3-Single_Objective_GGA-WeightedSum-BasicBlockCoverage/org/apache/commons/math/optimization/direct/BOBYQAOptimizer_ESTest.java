/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:27:05 UTC 2020
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
      int int0 = 20;
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(20);
      SumSincFunction sumSincFunction0 = new SumSincFunction(0.0);
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray0 = new double[7];
      doubleArray0[0] = 345.694;
      doubleArray0[1] = (double) 650;
      doubleArray0[2] = (double) 217;
      doubleArray0[3] = 345.694;
      doubleArray0[4] = (-38.60974855367);
      doubleArray0[5] = 345.694;
      doubleArray0[6] = (double) 20;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(217, sumSincFunction0, goalType0, doubleArray0);
  }
}
