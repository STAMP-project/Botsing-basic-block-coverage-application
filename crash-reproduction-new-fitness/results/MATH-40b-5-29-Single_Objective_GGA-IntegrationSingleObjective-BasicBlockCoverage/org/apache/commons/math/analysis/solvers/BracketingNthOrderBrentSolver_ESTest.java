/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:36:31 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SinFunction;
import org.apache.commons.math.analysis.UnivariateFunction;
import org.apache.commons.math.analysis.solvers.AllowedSolution;
import org.apache.commons.math.analysis.solvers.BracketingNthOrderBrentSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BracketingNthOrderBrentSolver_ESTest extends BracketingNthOrderBrentSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BracketingNthOrderBrentSolver bracketingNthOrderBrentSolver0 = new BracketingNthOrderBrentSolver();
      UnivariateFunction univariateFunction0 = mock(UnivariateFunction.class, new ViolatedAssumptionAnswer());
      doReturn(0.0).when(univariateFunction0).value(anyDouble());
      AllowedSolution allowedSolution0 = AllowedSolution.ANY_SIDE;
      bracketingNthOrderBrentSolver0.solve(4, univariateFunction0, 0.0, 2978.3892590729897, 1.0, allowedSolution0);
      SinFunction sinFunction0 = new SinFunction();
      // Undeclared exception!
      bracketingNthOrderBrentSolver0.solve(4, (UnivariateFunction) sinFunction0, (-2420.00283869027), 2978.3892590729897, 1.0, allowedSolution0);
  }
}
