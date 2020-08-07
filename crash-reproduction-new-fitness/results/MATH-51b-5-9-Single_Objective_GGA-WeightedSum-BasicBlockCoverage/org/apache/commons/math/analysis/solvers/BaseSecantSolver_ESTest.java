/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:40:34 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.QuinticFunction;
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
      double double0 = BaseSecantSolver.DEFAULT_ABSOLUTE_ACCURACY;
      IllinoisSolver illinoisSolver0 = new IllinoisSolver((-2548.4708645194));
      illinoisSolver0.getMin();
      IllinoisSolver illinoisSolver1 = new IllinoisSolver(0.19454770798898718, 2.7553817452272217E-6);
      QuinticFunction quinticFunction0 = new QuinticFunction();
      AllowedSolution allowedSolution0 = AllowedSolution.LEFT_SIDE;
      illinoisSolver1.solve(487, (UnivariateRealFunction) quinticFunction0, (-2402.17117675499), 1022.909, (-726.8032634507), allowedSolution0);
      PegasusSolver pegasusSolver1 = new PegasusSolver(0.19454770798898718, 0.0, (-2548.4708645194));
      // Undeclared exception!
      pegasusSolver1.solve(10, (UnivariateRealFunction) quinticFunction0, (-2402.17117675499), 752.17439011, allowedSolution0);
  }
}
