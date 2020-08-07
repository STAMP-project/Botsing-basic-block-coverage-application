/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 23:01:36 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Comparator;
import org.apache.commons.math.analysis.MultivariateVectorialFunction;
import org.apache.commons.math.optimization.OptimizationException;
import org.apache.commons.math.optimization.RealPointValuePair;
import org.apache.commons.math.optimization.SimpleScalarValueChecker;
import org.apache.commons.math.optimization.direct.MultiDirectional;
import org.apache.commons.math.optimization.direct.NelderMead;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DirectSearchOptimizer_ESTest extends DirectSearchOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = 0.0;
      MultiDirectional multiDirectional0 = new MultiDirectional(0.0, 0.0);
      SimpleScalarValueChecker simpleScalarValueChecker0 = new SimpleScalarValueChecker();
      NelderMead nelderMead0 = new NelderMead();
      nelderMead0.getIterations();
      nelderMead0.setMaxIterations(0);
      MultivariateVectorialFunction multivariateVectorialFunction0 = mock(MultivariateVectorialFunction.class, new ViolatedAssumptionAnswer());
      Comparator<RealPointValuePair> comparator0 = (Comparator<RealPointValuePair>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      nelderMead0.setMaxIterations((-219));
      double[] doubleArray0 = new double[4];
      nelderMead0.setMaxEvaluations(0);
      try { 
        nelderMead0.incrementIterationsCounter();
        fail("Expecting exception: OptimizationException");
      
      } catch(OptimizationException e) {
         //
         // org.apache.commons.math.MaxIterationsExceededException: Maximal number of iterations (-219) exceeded
         //
         verifyException("org.apache.commons.math.optimization.direct.DirectSearchOptimizer", e);
      }
  }
}
