/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:35:04 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SinFunction;
import org.apache.commons.math.analysis.UnivariateFunction;
import org.apache.commons.math.analysis.solvers.MullerSolver;
import org.apache.commons.math.analysis.solvers.SecantSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      int int0 = (-4185);
      SecantSolver secantSolver0 = new SecantSolver();
      MullerSolver mullerSolver0 = new MullerSolver((-3291.28648));
      SinFunction sinFunction0 = new SinFunction();
      // Undeclared exception!
      mullerSolver0.solve((-4185), (UnivariateFunction) sinFunction0, (-3291.28648));
  }
}
