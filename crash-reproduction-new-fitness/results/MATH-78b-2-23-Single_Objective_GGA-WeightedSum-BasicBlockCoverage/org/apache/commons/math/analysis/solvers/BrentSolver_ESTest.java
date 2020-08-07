/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:40:56 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.QuinticFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.BrentSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BrentSolver_ESTest extends BrentSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      UnivariateRealFunction univariateRealFunction0 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      BrentSolver brentSolver0 = new BrentSolver();
      brentSolver0.resetMaximalIterationCount();
      brentSolver0.setResult((-1.0), (-1.0), 0);
      UnivariateRealFunction univariateRealFunction1 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      doReturn(0.0, 0.0).when(univariateRealFunction1).value(anyDouble());
      brentSolver0.solve(univariateRealFunction1, (-1.0), 924.7606797);
      brentSolver0.resetMaximalIterationCount();
      brentSolver0.setResult((-1.0), (-1.0), 0);
      QuinticFunction quinticFunction0 = new QuinticFunction();
      UnivariateRealFunction univariateRealFunction2 = quinticFunction0.derivative();
      // Undeclared exception!
      brentSolver0.solve(univariateRealFunction2, 1.0E-6, 1718.36111662);
  }
}
