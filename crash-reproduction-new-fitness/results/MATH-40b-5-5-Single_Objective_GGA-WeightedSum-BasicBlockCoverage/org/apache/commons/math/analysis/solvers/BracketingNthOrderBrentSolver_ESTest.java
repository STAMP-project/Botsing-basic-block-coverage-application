/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:32:29 UTC 2020
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
      BracketingNthOrderBrentSolver bracketingNthOrderBrentSolver1 = new BracketingNthOrderBrentSolver(1.3330593446880812E-5, 5);
      int int0 = 1930;
      SinFunction sinFunction0 = new SinFunction();
      sinFunction0.derivative();
      AllowedSolution allowedSolution0 = AllowedSolution.LEFT_SIDE;
      // Undeclared exception!
      bracketingNthOrderBrentSolver1.solve(5, (UnivariateFunction) sinFunction0, (-4360.0), 0.5, (-115.908756638), allowedSolution0);
  }
}
