/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 03:37:06 UTC 2020
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
      doReturn(0.0, 0.0).when(univariateRealFunction0).value(anyDouble());
      BrentSolver brentSolver0 = new BrentSolver(univariateRealFunction0);
      brentSolver0.setFunctionValueAccuracy((-1316.6771));
      int int0 = 605;
      brentSolver0.setMaximalIterationCount(605);
      brentSolver0.getRelativeAccuracy();
      double double0 = 0.0;
      brentSolver0.setResult(0.0, (-413));
      brentSolver0.resetFunctionValueAccuracy();
      brentSolver0.setMaximalIterationCount(605);
      brentSolver0.getFunctionValueAccuracy();
      // Undeclared exception!
      brentSolver0.solve((double) (-413), 1080.47126416786);
  }
}
