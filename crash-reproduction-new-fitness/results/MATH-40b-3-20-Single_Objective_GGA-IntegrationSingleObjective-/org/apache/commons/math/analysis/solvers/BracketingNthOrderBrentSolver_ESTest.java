/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:40:32 UTC 2020
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
      int int0 = 2403;
      BracketingNthOrderBrentSolver bracketingNthOrderBrentSolver1 = new BracketingNthOrderBrentSolver((-2492.873), 2403);
      SinFunction sinFunction0 = new SinFunction();
      sinFunction0.derivative();
      double double0 = 0.0625;
      sinFunction0.value(0.0625);
      double double1 = (-0.0625);
      bracketingNthOrderBrentSolver1.solve(2403, (UnivariateFunction) sinFunction0, (-1867.45), 0.0625, (-0.0625));
      AllowedSolution allowedSolution0 = AllowedSolution.ABOVE_SIDE;
      // Undeclared exception!
      bracketingNthOrderBrentSolver1.solve(1099, (UnivariateFunction) sinFunction0, 0.0624593178423802, 1564.0, allowedSolution0);
  }
}
