/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 21:05:26 UTC 2021
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.SinFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.IllinoisSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Expm1Function expm1Function0 = new Expm1Function();
      SinFunction sinFunction0 = new SinFunction();
      IllinoisSolver illinoisSolver0 = new IllinoisSolver(1.4456468917292502E-16, 1913.3720465, (-1102.39373406027));
      illinoisSolver0.getEvaluations();
      // Undeclared exception!
      illinoisSolver0.solve(2, (UnivariateRealFunction) expm1Function0, (-1546.28684438), 4.930637065450937E-4, 0.0);
  }
}
