/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:40:33 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.QuinticFunction;
import org.apache.commons.math.analysis.SinFunction;
import org.apache.commons.math.analysis.UnivariateFunction;
import org.apache.commons.math.analysis.XMinus5Function;
import org.apache.commons.math.analysis.solvers.BracketingNthOrderBrentSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BracketingNthOrderBrentSolver_ESTest extends BracketingNthOrderBrentSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BracketingNthOrderBrentSolver bracketingNthOrderBrentSolver0 = new BracketingNthOrderBrentSolver();
      bracketingNthOrderBrentSolver0.getAbsoluteAccuracy();
      QuinticFunction quinticFunction0 = new QuinticFunction();
      SinFunction sinFunction0 = new SinFunction();
      UnivariateFunction univariateFunction0 = sinFunction0.derivative();
      bracketingNthOrderBrentSolver0.solve(399, univariateFunction0, (-3.356118100840571E-7), (double) 399);
      bracketingNthOrderBrentSolver0.doSolve();
      QuinticFunction quinticFunction1 = new QuinticFunction();
      XMinus5Function xMinus5Function0 = new XMinus5Function();
      SinFunction sinFunction1 = new SinFunction();
      UnivariateFunction univariateFunction1 = sinFunction0.derivative();
      // Undeclared exception!
      bracketingNthOrderBrentSolver0.solve(5, univariateFunction1, 699.63, 1050.40114);
  }
}
