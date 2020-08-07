/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:59:31 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.solvers.NewtonSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = 0.0;
      double double1 = 27.74493574743586;
      double double2 = 147.1751595942082;
      double double3 = 2.0;
      double double4 = 4031.0;
      NewtonSolver newtonSolver0 = new NewtonSolver();
      // Undeclared exception!
      newtonSolver0.doSolve();
  }
}
