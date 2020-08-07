/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:28:23 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SumSincFunction;
import org.apache.commons.math.optimization.GoalType;
import org.apache.commons.math.optimization.direct.BOBYQAOptimizer;
import org.apache.commons.math.random.Well1024a;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractMultivariateOptimizer_ESTest extends BaseAbstractMultivariateOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      GoalType goalType0 = GoalType.MINIMIZE;
      int int0 = 44;
      double[] doubleArray0 = new double[13];
      SumSincFunction sumSincFunction0 = new SumSincFunction(44);
      SumSincFunction sumSincFunction1 = new SumSincFunction(44);
      sumSincFunction0.gradient();
      sumSincFunction0.gradient();
      Well1024a well1024a0 = new Well1024a((long) 44);
      sumSincFunction0.gradient();
      sumSincFunction0.gradient();
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(44, (-597.6087023277977), 0.0);
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(44, sumSincFunction0, goalType0, doubleArray0);
  }
}
