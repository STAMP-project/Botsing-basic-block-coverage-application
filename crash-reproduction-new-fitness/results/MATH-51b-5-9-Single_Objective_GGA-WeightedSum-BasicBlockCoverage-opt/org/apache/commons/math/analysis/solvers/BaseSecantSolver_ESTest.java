/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 21:05:32 UTC 2021
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.SincFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.AllowedSolution;
import org.apache.commons.math.analysis.solvers.BaseSecantSolver;
import org.apache.commons.math.analysis.solvers.IllinoisSolver;
import org.apache.commons.math.analysis.solvers.PegasusSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PegasusSolver pegasusSolver0 = new PegasusSolver();
      pegasusSolver0.getMax();
      IllinoisSolver illinoisSolver0 = new IllinoisSolver((-15.920906403605297));
      Expm1Function expm1Function0 = new Expm1Function();
      AllowedSolution allowedSolution0 = AllowedSolution.BELOW_SIDE;
      double double0 = BaseSecantSolver.DEFAULT_ABSOLUTE_ACCURACY;
      pegasusSolver0.isSequence(15.261391989549368, 1885.7068, 0.0);
      double double1 = BaseSecantSolver.DEFAULT_ABSOLUTE_ACCURACY;
      SincFunction sincFunction0 = new SincFunction();
      AllowedSolution allowedSolution1 = AllowedSolution.LEFT_SIDE;
      pegasusSolver0.solve(3, (UnivariateRealFunction) expm1Function0, (double) (-2093), 0.0, 1.0E-6, allowedSolution1);
      // Undeclared exception!
      pegasusSolver0.solve(750, (UnivariateRealFunction) expm1Function0, (-3868.4717), 458.6206, 15.261391989549368, allowedSolution0);
  }
}
