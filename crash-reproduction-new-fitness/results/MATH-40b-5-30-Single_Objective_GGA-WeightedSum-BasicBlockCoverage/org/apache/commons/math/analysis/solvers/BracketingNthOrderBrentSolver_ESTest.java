/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:59:47 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SinFunction;
import org.apache.commons.math.analysis.UnivariateFunction;
import org.apache.commons.math.analysis.solvers.AllowedSolution;
import org.apache.commons.math.analysis.solvers.BracketingNthOrderBrentSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BracketingNthOrderBrentSolver_ESTest extends BracketingNthOrderBrentSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BracketingNthOrderBrentSolver bracketingNthOrderBrentSolver0 = new BracketingNthOrderBrentSolver();
      SinFunction sinFunction0 = new SinFunction();
      UnivariateFunction univariateFunction0 = sinFunction0.derivative();
      sinFunction0.derivative();
      AllowedSolution allowedSolution0 = AllowedSolution.RIGHT_SIDE;
      // Undeclared exception!
      bracketingNthOrderBrentSolver0.solve(3, univariateFunction0, (double) 3, 2979.362, 1619.16, allowedSolution0);
  }
}
