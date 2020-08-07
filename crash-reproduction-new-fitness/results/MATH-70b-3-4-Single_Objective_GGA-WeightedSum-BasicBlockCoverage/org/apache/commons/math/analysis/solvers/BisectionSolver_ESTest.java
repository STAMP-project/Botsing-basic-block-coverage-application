/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 12:40:28 UTC 2020
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
      UnivariateRealFunction univariateRealFunction0 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      UnivariateRealFunction univariateRealFunction1 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      double double0 = 0.0;
      bisectionSolver0.verifyInterval(0.0, 1.0);
      bisectionSolver0.getAbsoluteAccuracy();
      UnivariateRealFunction univariateRealFunction2 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      double double1 = 0.0;
      // Undeclared exception!
      bisectionSolver0.solve(univariateRealFunction2, (-3215.8059225), 0.0, 0.0);
  }
}
