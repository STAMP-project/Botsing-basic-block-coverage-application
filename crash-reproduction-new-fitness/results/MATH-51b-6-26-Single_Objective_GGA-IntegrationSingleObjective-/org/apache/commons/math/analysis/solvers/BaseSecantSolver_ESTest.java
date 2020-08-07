/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:50:20 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.PegasusSolver;
import org.apache.commons.math.analysis.solvers.RegulaFalsiSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PegasusSolver pegasusSolver0 = new PegasusSolver((-1127.5210687823549));
      Expm1Function expm1Function0 = new Expm1Function();
      expm1Function0.derivative();
      double double0 = (-0.996842971736072);
      double double1 = 1151.67570365471;
      RegulaFalsiSolver regulaFalsiSolver0 = new RegulaFalsiSolver(1151.67570365471, 1151.67570365471);
      // Undeclared exception!
      regulaFalsiSolver0.solve(175, (UnivariateRealFunction) expm1Function0, (-0.996842971736072), 1151.67570365471, 3.06398194336125E15);
  }
}
