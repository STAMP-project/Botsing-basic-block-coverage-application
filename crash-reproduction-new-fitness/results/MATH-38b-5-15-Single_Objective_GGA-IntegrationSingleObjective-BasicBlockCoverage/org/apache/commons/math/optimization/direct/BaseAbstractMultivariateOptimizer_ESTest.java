/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:13:39 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SumSincFunction;
import org.apache.commons.math.optimization.GoalType;
import org.apache.commons.math.optimization.RealPointValuePair;
import org.apache.commons.math.optimization.direct.BOBYQAOptimizer;
import org.apache.commons.math.optimization.direct.SimplexOptimizer;
import org.apache.commons.math.random.ISAACRandom;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractMultivariateOptimizer_ESTest extends BaseAbstractMultivariateOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      RealPointValuePair[] realPointValuePairArray0 = new RealPointValuePair[7];
      ISAACRandom iSAACRandom0 = new ISAACRandom();
      SimplexOptimizer simplexOptimizer0 = new SimplexOptimizer();
      double double0 = 1115.43164245;
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(29, 29, 1115.43164245);
      SumSincFunction sumSincFunction0 = new SumSincFunction((-0.6084009650675007));
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray0 = new double[17];
      doubleArray0[0] = (double) 29;
      double[] doubleArray1 = new double[8];
      doubleArray1[2] = (double) 29;
      doubleArray1[3] = 1115.43164245;
      doubleArray1[4] = (double) 1396;
      doubleArray1[5] = (-20.61241888409512);
      doubleArray1[7] = 1115.43164245;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(1396, sumSincFunction0, goalType0, doubleArray1);
  }
}
