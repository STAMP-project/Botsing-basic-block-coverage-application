/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:19:21 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.solvers.BracketingNthOrderBrentSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = Double.NaN;
      int int0 = 2;
      BracketingNthOrderBrentSolver bracketingNthOrderBrentSolver0 = new BracketingNthOrderBrentSolver(Double.NaN, 2);
      // Undeclared exception!
      bracketingNthOrderBrentSolver0.incrementEvaluationCount();
  }
}
