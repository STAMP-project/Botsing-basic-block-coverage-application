/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:14:59 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
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
      BracketingNthOrderBrentSolver bracketingNthOrderBrentSolver1 = new BracketingNthOrderBrentSolver();
      BracketingNthOrderBrentSolver bracketingNthOrderBrentSolver2 = new BracketingNthOrderBrentSolver();
      bracketingNthOrderBrentSolver2.getMaxEvaluations();
      int int0 = 2615;
      BracketingNthOrderBrentSolver bracketingNthOrderBrentSolver3 = new BracketingNthOrderBrentSolver((-1980.5), 4);
      SinFunction sinFunction0 = new SinFunction();
      UnivariateFunction univariateFunction0 = sinFunction0.derivative();
      double double0 = (-2183.5400177);
      bracketingNthOrderBrentSolver3.setup(10, univariateFunction0, (-1980.5), 1672.9312747200672, 0.0);
      XMinus5Function xMinus5Function0 = new XMinus5Function();
      // Undeclared exception!
      bracketingNthOrderBrentSolver3.doSolve();
  }
}
