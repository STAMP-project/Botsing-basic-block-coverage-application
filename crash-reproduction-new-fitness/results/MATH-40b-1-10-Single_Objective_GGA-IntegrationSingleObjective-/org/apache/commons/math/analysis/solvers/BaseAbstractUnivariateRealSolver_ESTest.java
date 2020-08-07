/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:16:24 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.solvers.BaseSecantSolver;
import org.apache.commons.math.analysis.solvers.BrentSolver;
import org.apache.commons.math.analysis.solvers.MullerSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BrentSolver brentSolver0 = new BrentSolver();
      double double0 = (-2443.54);
      double double1 = 0.5;
      double double2 = BaseSecantSolver.DEFAULT_ABSOLUTE_ACCURACY;
      double double3 = 0.0;
      MullerSolver mullerSolver0 = new MullerSolver(4902.1633602, (-2443.54));
      // Undeclared exception!
      mullerSolver0.incrementEvaluationCount();
  }
}
