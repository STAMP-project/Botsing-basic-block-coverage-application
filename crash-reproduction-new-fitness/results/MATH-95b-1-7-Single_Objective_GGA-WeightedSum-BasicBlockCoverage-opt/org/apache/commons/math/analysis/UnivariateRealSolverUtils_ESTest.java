/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 21:32:47 UTC 2021
 */

package org.apache.commons.math.analysis;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.UnivariateRealSolverUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class UnivariateRealSolverUtils_ESTest extends UnivariateRealSolverUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Expm1Function expm1Function0 = new Expm1Function();
      double double0 = (-1377.52085267);
      int int0 = 383;
      // Undeclared exception!
      UnivariateRealSolverUtils.bracket((UnivariateRealFunction) expm1Function0, (-1377.52085267), (-1377.52085267), (-1377.52085267), 383);
  }
}
