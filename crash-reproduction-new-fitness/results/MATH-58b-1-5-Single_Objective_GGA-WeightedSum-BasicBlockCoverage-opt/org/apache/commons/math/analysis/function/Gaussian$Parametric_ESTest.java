/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 21:10:36 UTC 2021
 */

package org.apache.commons.math.analysis.function;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.function.Gaussian;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Gaussian$Parametric_ESTest extends Gaussian$Parametric_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Gaussian.Parametric gaussian_Parametric0 = new Gaussian.Parametric();
      double double0 = (-2400.1737427731373);
      double[] doubleArray0 = new double[3];
      doubleArray0[0] = (-2400.1737427731373);
      doubleArray0[1] = (-2400.1737427731373);
      doubleArray0[2] = (-2400.1737427731373);
      // Undeclared exception!
      gaussian_Parametric0.gradient((-2400.1737427731373), doubleArray0);
  }
}
