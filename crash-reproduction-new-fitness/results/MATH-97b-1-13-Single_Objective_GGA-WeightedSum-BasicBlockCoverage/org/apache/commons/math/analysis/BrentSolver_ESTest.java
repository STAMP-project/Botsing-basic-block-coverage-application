/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 22:27:12 UTC 2020
 */

package org.apache.commons.math.analysis;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.BrentSolver;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BrentSolver_ESTest extends BrentSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      UnivariateRealFunction univariateRealFunction0 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      BrentSolver brentSolver0 = new BrentSolver(univariateRealFunction0);
      UnivariateRealFunction univariateRealFunction1 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      doReturn(0.0, 0.0).when(univariateRealFunction1).value(anyDouble());
      BrentSolver brentSolver1 = new BrentSolver(univariateRealFunction1);
      brentSolver0.resetAbsoluteAccuracy();
      UnivariateRealFunction univariateRealFunction2 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      doReturn(0.0, 0.0).when(univariateRealFunction2).value(anyDouble());
      brentSolver1.isBracketing(0.0, 1.0E-6, univariateRealFunction2);
      UnivariateRealFunction univariateRealFunction3 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      doReturn(0.0, 0.0).when(univariateRealFunction3).value(anyDouble());
      brentSolver1.isBracketing(0.0, Double.NaN, univariateRealFunction3);
      // Undeclared exception!
      brentSolver1.solve(0.0, 1.0E-6);
  }
}
