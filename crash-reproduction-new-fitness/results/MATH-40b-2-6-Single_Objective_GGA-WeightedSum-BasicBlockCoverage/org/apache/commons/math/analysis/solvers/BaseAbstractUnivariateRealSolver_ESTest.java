/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:32:12 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.solvers.RiddersSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = (-1595.0043683472702);
      double double1 = 1330.206047984202;
      int int0 = 3497;
      RiddersSolver riddersSolver0 = new RiddersSolver(0.0);
      riddersSolver0.getEvaluations();
      riddersSolver0.getEvaluations();
      // Undeclared exception!
      riddersSolver0.doSolve();
  }
}
