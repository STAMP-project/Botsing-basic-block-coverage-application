/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:35:34 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.QuinticFunction;
import org.apache.commons.math.analysis.SincFunction;
import org.apache.commons.math.analysis.UnivariateFunction;
import org.apache.commons.math.analysis.solvers.BracketingNthOrderBrentSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BracketingNthOrderBrentSolver_ESTest extends BracketingNthOrderBrentSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BracketingNthOrderBrentSolver bracketingNthOrderBrentSolver0 = new BracketingNthOrderBrentSolver();
      int int0 = 95;
      SincFunction sincFunction0 = new SincFunction();
      bracketingNthOrderBrentSolver0.solve(95, (UnivariateFunction) sincFunction0, 4.378854472088278, 622.9995);
      QuinticFunction quinticFunction0 = new QuinticFunction();
      quinticFunction0.derivative();
      SincFunction sincFunction1 = new SincFunction();
      int int1 = 2;
      // Undeclared exception!
      bracketingNthOrderBrentSolver0.solve(2, (UnivariateFunction) sincFunction1, 4.378854472088278, 502.6548245743682);
  }
}
