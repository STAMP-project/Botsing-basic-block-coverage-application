/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:59:51 UTC 2020
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
      int int0 = 170;
      BracketingNthOrderBrentSolver bracketingNthOrderBrentSolver0 = new BracketingNthOrderBrentSolver(Double.NaN, Double.NaN, 0.0, 170);
      AllowedSolution allowedSolution0 = AllowedSolution.ANY_SIDE;
      SinFunction sinFunction0 = new SinFunction();
      // Undeclared exception!
      bracketingNthOrderBrentSolver0.solve(170, (UnivariateFunction) sinFunction0, (double) 170, Double.NaN, 4577.726041279148, allowedSolution0);
  }
}
