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
      Gaussian.Parametric gaussian_Parametric1 = new Gaussian.Parametric();
      double[] doubleArray0 = new double[3];
      doubleArray0[0] = 3.9736429850260626E-8;
      doubleArray0[1] = 3.9736429850260626E-8;
      doubleArray0[2] = 3.9736429850260626E-8;
      gaussian_Parametric0.value(3.9736429850260626E-8, doubleArray0);
      Gaussian.Parametric gaussian_Parametric2 = new Gaussian.Parametric();
      double[] doubleArray1 = gaussian_Parametric1.gradient(3.9736429850260626E-8, doubleArray0);
      // Undeclared exception!
      gaussian_Parametric1.value(3.9736429850260626E-8, doubleArray1);
  }
}
