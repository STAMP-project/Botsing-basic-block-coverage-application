/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:26:27 UTC 2020
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
      double[] doubleArray0 = new double[3];
      doubleArray0[0] = 325.1;
      doubleArray0[1] = 325.1;
      doubleArray0[2] = 325.1;
      gaussian_Parametric0.value(325.1, doubleArray0);
      gaussian_Parametric0.value(325.1, doubleArray0);
      double[] doubleArray1 = gaussian_Parametric0.gradient(325.1, doubleArray0);
      // Undeclared exception!
      gaussian_Parametric0.value(2763.988125806004, doubleArray1);
  }
}
