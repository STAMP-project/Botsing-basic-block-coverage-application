/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:29:08 UTC 2020
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(8);
      SumSincFunction sumSincFunction0 = new SumSincFunction((-2300.983049076));
      GoalType goalType0 = GoalType.MAXIMIZE;
      double[] doubleArray0 = new double[3];
      doubleArray0[0] = (-3667.2514134592607);
      doubleArray0[1] = (double) 8;
      doubleArray0[2] = (-2300.983049076);
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(8, sumSincFunction0, goalType0, doubleArray0);
  }
}
