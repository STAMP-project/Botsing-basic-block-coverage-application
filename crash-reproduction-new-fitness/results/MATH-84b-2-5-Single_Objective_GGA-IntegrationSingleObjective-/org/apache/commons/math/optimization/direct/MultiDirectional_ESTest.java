/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 23:02:42 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Comparator;
import org.apache.commons.math.optimization.OptimizationException;
import org.apache.commons.math.optimization.RealConvergenceChecker;
import org.apache.commons.math.optimization.RealPointValuePair;
import org.apache.commons.math.optimization.direct.MultiDirectional;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class MultiDirectional_ESTest extends MultiDirectional_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MultiDirectional multiDirectional0 = new MultiDirectional((-556.259), 3350.817609936);
      multiDirectional0.setMaxIterations((-2499));
      multiDirectional0.getIterations();
      RealConvergenceChecker realConvergenceChecker0 = multiDirectional0.getConvergenceChecker();
      double[] doubleArray0 = new double[5];
      doubleArray0[0] = 3350.817609936;
      doubleArray0[1] = (double) (-2499);
      doubleArray0[2] = 3350.817609936;
      doubleArray0[3] = (double) (-2499);
      doubleArray0[4] = (double) 0;
      double[] doubleArray1 = new double[9];
      doubleArray1[0] = (double) (-2499);
      doubleArray1[1] = (-556.259);
      doubleArray1[2] = (double) 0;
      doubleArray1[3] = (double) 0;
      doubleArray1[4] = (double) 0;
      doubleArray1[5] = (double) (-2499);
      doubleArray1[6] = (double) (-2499);
      doubleArray1[7] = (-556.259);
      doubleArray1[8] = 3350.817609936;
      double[] doubleArray2 = new double[1];
      doubleArray2[0] = 3350.817609936;
      multiDirectional0.getMaxEvaluations();
      multiDirectional0.setConvergenceChecker(realConvergenceChecker0);
      multiDirectional0.getConvergenceChecker();
      multiDirectional0.setConvergenceChecker(realConvergenceChecker0);
      Comparator<RealPointValuePair> comparator0 = (Comparator<RealPointValuePair>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      try { 
        multiDirectional0.iterateSimplex(comparator0);
        fail("Expecting exception: OptimizationException");
      
      } catch(OptimizationException e) {
         //
         // org.apache.commons.math.MaxIterationsExceededException: Maximal number of iterations (-2,499) exceeded
         //
         verifyException("org.apache.commons.math.optimization.direct.DirectSearchOptimizer", e);
      }
  }
}
