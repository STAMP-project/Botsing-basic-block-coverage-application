/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:35:48 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.QuinticFunction;
import org.apache.commons.math.analysis.UnivariateFunction;
import org.apache.commons.math.analysis.solvers.BracketingNthOrderBrentSolver;
import org.apache.commons.math.analysis.solvers.SecantSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = 6.6926396887292805;
      double double1 = SecantSolver.DEFAULT_ABSOLUTE_ACCURACY;
      double double2 = 0.5022931153641605;
      double double3 = (-32.6984898060713);
      BracketingNthOrderBrentSolver bracketingNthOrderBrentSolver0 = new BracketingNthOrderBrentSolver();
      QuinticFunction quinticFunction0 = new QuinticFunction();
      int int0 = 1150;
      bracketingNthOrderBrentSolver0.setup(1150, quinticFunction0, (-6.382772018021702), 6.6926396887292805, 1.0E-6);
      quinticFunction0.derivative();
      // Undeclared exception!
      bracketingNthOrderBrentSolver0.solve(7, (UnivariateFunction) quinticFunction0, (-6.382772018021702), 3.7179924024793253E227, 6.6926396887292805);
  }
}
