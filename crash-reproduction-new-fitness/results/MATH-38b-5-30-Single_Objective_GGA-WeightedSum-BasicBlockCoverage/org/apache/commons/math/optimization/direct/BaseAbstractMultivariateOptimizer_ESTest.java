/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:57:25 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SumSincFunction;
import org.apache.commons.math.optimization.GoalType;
import org.apache.commons.math.optimization.RealPointValuePair;
import org.apache.commons.math.optimization.direct.BOBYQAOptimizer;
import org.apache.commons.math.random.JDKRandomGenerator;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractMultivariateOptimizer_ESTest extends BaseAbstractMultivariateOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      JDKRandomGenerator jDKRandomGenerator0 = new JDKRandomGenerator();
      RealPointValuePair[] realPointValuePairArray0 = new RealPointValuePair[6];
      int int0 = 19;
      jDKRandomGenerator0.longs();
      int[] intArray0 = new int[9];
      intArray0[2] = 19;
      intArray0[1] = 19;
      intArray0[2] = 19;
      intArray0[4] = 19;
      intArray0[2] = 19;
      intArray0[7] = 19;
      intArray0[8] = 19;
      jDKRandomGenerator0.setSeed(intArray0);
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(19, 19, 19);
      double[] doubleArray0 = new double[8];
      SumSincFunction sumSincFunction0 = new SumSincFunction(19);
      sumSincFunction0.value(doubleArray0);
      GoalType goalType0 = GoalType.MINIMIZE;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(1213, sumSincFunction0, goalType0, doubleArray0);
  }
}
