/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 03:36:13 UTC 2020
 */

package org.apache.commons.math.analysis;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.UnivariateRealSolverUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class UnivariateRealSolverUtils_ESTest extends UnivariateRealSolverUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      UnivariateRealFunction univariateRealFunction0 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      doReturn(0.0, 0.0).when(univariateRealFunction0).value(anyDouble());
      double double0 = (-1004.26013);
      UnivariateRealSolverUtils.solve(univariateRealFunction0, (-1004.26013), 1942.0);
      UnivariateRealFunction univariateRealFunction1 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      // Undeclared exception!
      UnivariateRealSolverUtils.bracket(univariateRealFunction1, 1942.0, (-1004.26013), 1433.19297);
  }
}
