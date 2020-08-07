/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:41:10 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.MonitoredFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.XMinus5Function;
import org.apache.commons.math.analysis.solvers.AllowedSolution;
import org.apache.commons.math.analysis.solvers.PegasusSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PegasusSolver pegasusSolver0 = new PegasusSolver();
      int int0 = (-979);
      AllowedSolution allowedSolution0 = AllowedSolution.ABOVE_SIDE;
      Expm1Function expm1Function0 = new Expm1Function();
      expm1Function0.derivative();
      expm1Function0.derivative();
      XMinus5Function xMinus5Function0 = new XMinus5Function();
      MonitoredFunction monitoredFunction0 = new MonitoredFunction((UnivariateRealFunction) null);
      AllowedSolution allowedSolution1 = AllowedSolution.ABOVE_SIDE;
      // Undeclared exception!
      pegasusSolver0.solve(2190, (UnivariateRealFunction) expm1Function0, (-2306.246620063), (double) 2190, (-2306.246620063), allowedSolution1);
  }
}
