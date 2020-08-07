/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:44:24 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SinFunction;
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
      SinFunction sinFunction0 = new SinFunction();
      PegasusSolver pegasusSolver0 = new PegasusSolver((-1040.0), 0.0);
      int int0 = 1195;
      IllinoisSolver illinoisSolver0 = new IllinoisSolver(Double.POSITIVE_INFINITY, (-0.46025003678301546));
      double double0 = BaseSecantSolver.DEFAULT_ABSOLUTE_ACCURACY;
      SinFunction sinFunction1 = new SinFunction();
      AllowedSolution allowedSolution0 = AllowedSolution.ANY_SIDE;
      // Undeclared exception!
      pegasusSolver0.solve(1195, (UnivariateRealFunction) sinFunction0, (-764.199054225), 1060.96986238275, (-2207.7), allowedSolution0);
  }
}
