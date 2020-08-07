/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:36:54 UTC 2020
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
      int int0 = 11;
      double double0 = 3478.904925;
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(11, 3478.904925, 1974.012405879);
      int int1 = 680;
      double double1 = 0.0;
      SumSincFunction sumSincFunction0 = new SumSincFunction(0.0);
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray0 = new double[9];
      double[] doubleArray1 = new double[4];
      doubleArray1[0] = 1974.012405879;
      doubleArray1[1] = 0.0;
      doubleArray1[2] = (double) 11;
      doubleArray1[3] = 0.0;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(11, sumSincFunction0, goalType0, doubleArray1);
  }
}
