/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:58:50 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.optimization.OptimizationException;
import org.apache.commons.math.optimization.RealConvergenceChecker;
import org.apache.commons.math.optimization.direct.MultiDirectional;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DirectSearchOptimizer_ESTest extends DirectSearchOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MultiDirectional multiDirectional0 = new MultiDirectional();
      RealConvergenceChecker realConvergenceChecker0 = mock(RealConvergenceChecker.class, new ViolatedAssumptionAnswer());
      doReturn((String) null).when(realConvergenceChecker0).toString();
      multiDirectional0.setConvergenceChecker(realConvergenceChecker0);
      RealConvergenceChecker realConvergenceChecker1 = multiDirectional0.getConvergenceChecker();
      multiDirectional0.setConvergenceChecker(realConvergenceChecker1);
      int int0 = 0;
      multiDirectional0.setMaxIterations(0);
      try { 
        multiDirectional0.incrementIterationsCounter();
        fail("Expecting exception: OptimizationException");
      
      } catch(OptimizationException e) {
         //
         // org.apache.commons.math.MaxIterationsExceededException: Maximal number of iterations (0) exceeded
         //
         verifyException("org.apache.commons.math.optimization.direct.DirectSearchOptimizer", e);
      }
  }
}
