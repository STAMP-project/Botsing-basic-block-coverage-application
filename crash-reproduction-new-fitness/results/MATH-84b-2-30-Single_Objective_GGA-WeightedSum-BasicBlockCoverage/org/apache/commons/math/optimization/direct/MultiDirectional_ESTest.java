/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:44:28 UTC 2020
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
      MultiDirectional multiDirectional0 = new MultiDirectional(2196.75033852481, (-1207.427408568));
      multiDirectional0.getIterations();
      multiDirectional0.incrementIterationsCounter();
      multiDirectional0.incrementIterationsCounter();
      multiDirectional0.setMaxEvaluations(1);
      RealConvergenceChecker realConvergenceChecker0 = multiDirectional0.getConvergenceChecker();
      multiDirectional0.getIterations();
      multiDirectional0.setMaxIterations(2);
      multiDirectional0.getEvaluations();
      multiDirectional0.getEvaluations();
      multiDirectional0.setConvergenceChecker(realConvergenceChecker0);
      MultiDirectional multiDirectional1 = new MultiDirectional((-1.0), 2);
      MultiDirectional multiDirectional2 = new MultiDirectional();
      multiDirectional0.getEvaluations();
      multiDirectional1.getMaxIterations();
      int int0 = (-2569);
      multiDirectional1.setMaxIterations((-2569));
      multiDirectional0.setMaxEvaluations(0);
      Comparator<RealPointValuePair> comparator0 = (Comparator<RealPointValuePair>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      try { 
        multiDirectional1.iterateSimplex(comparator0);
        fail("Expecting exception: OptimizationException");
      
      } catch(OptimizationException e) {
         //
         // org.apache.commons.math.MaxIterationsExceededException: Maximal number of iterations (-2,569) exceeded
         //
         verifyException("org.apache.commons.math.optimization.direct.DirectSearchOptimizer", e);
      }
  }
}
