/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:59:40 UTC 2021
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.UnivariateFunction;
import org.apache.commons.math.analysis.solvers.MullerSolver;
import org.apache.commons.math.analysis.solvers.SecantSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      UnivariateFunction univariateFunction0 = mock(UnivariateFunction.class, new ViolatedAssumptionAnswer());
      MullerSolver mullerSolver0 = new MullerSolver(859.306821);
      SecantSolver secantSolver0 = new SecantSolver();
      // Undeclared exception!
      secantSolver0.doSolve();
  }
}
