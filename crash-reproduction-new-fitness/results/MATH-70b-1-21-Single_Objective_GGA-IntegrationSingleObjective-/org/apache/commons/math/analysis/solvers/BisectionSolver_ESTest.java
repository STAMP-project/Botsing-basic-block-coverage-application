/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:06:04 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.BisectionSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BisectionSolver_ESTest extends BisectionSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BisectionSolver bisectionSolver0 = new BisectionSolver();
      BisectionSolver bisectionSolver1 = new BisectionSolver();
      int int0 = 0;
      bisectionSolver1.setResult(0.0, 0);
      bisectionSolver1.getMaximalIterationCount();
      bisectionSolver1.getMaximalIterationCount();
      UnivariateRealFunction univariateRealFunction0 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      double double0 = 0.0;
      double double1 = 2961.004095383094;
      double double2 = 4200.920649;
      // Undeclared exception!
      bisectionSolver1.solve(univariateRealFunction0, 0.0, 2961.004095383094, 4200.920649);
  }
}
