/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:39:17 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.QuinticFunction;
import org.apache.commons.math.analysis.SincFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.AllowedSolution;
import org.apache.commons.math.analysis.solvers.BaseSecantSolver;
import org.apache.commons.math.analysis.solvers.RegulaFalsiSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = BaseSecantSolver.DEFAULT_ABSOLUTE_ACCURACY;
      QuinticFunction quinticFunction0 = new QuinticFunction();
      double double1 = (-586.30136762636);
      double double2 = (-1.0);
      quinticFunction0.derivative();
      quinticFunction0.derivative();
      RegulaFalsiSolver regulaFalsiSolver0 = new RegulaFalsiSolver((-2888.08), (-2372.93114883));
      SincFunction sincFunction0 = new SincFunction();
      AllowedSolution allowedSolution0 = AllowedSolution.RIGHT_SIDE;
      // Undeclared exception!
      regulaFalsiSolver0.solve(115, (UnivariateRealFunction) quinticFunction0, (-1325.378234495), 2591.419410744432, (-586.30136762636), allowedSolution0);
  }
}
