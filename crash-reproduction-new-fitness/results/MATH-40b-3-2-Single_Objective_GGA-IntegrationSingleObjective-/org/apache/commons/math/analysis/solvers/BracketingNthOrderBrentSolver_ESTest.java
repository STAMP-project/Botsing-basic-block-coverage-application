/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:15:10 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.MonitoredFunction;
import org.apache.commons.math.analysis.QuinticFunction;
import org.apache.commons.math.analysis.UnivariateFunction;
import org.apache.commons.math.analysis.XMinus5Function;
import org.apache.commons.math.analysis.solvers.AllowedSolution;
import org.apache.commons.math.analysis.solvers.BracketingNthOrderBrentSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BracketingNthOrderBrentSolver_ESTest extends BracketingNthOrderBrentSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = (-1694.06);
      double double1 = (-1518.204675);
      XMinus5Function xMinus5Function0 = new XMinus5Function();
      QuinticFunction quinticFunction0 = new QuinticFunction();
      MonitoredFunction monitoredFunction0 = new MonitoredFunction(quinticFunction0);
      monitoredFunction0.value(0.0);
      AllowedSolution allowedSolution0 = AllowedSolution.ANY_SIDE;
      BracketingNthOrderBrentSolver bracketingNthOrderBrentSolver0 = new BracketingNthOrderBrentSolver(0.0, (-0.9628947931621349), 130, 130);
      // Undeclared exception!
      bracketingNthOrderBrentSolver0.solve(8, (UnivariateFunction) monitoredFunction0, (-1474.9337344613573), 327.29926, 5.251554594269693E-10, allowedSolution0);
  }
}
