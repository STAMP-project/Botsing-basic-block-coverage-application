/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:32:56 UTC 2020
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
      Gaussian.Parametric gaussian_Parametric2 = new Gaussian.Parametric();
      double double0 = (-1372.637618619536);
      double[] doubleArray0 = new double[3];
      doubleArray0[0] = 2617.859185702037;
      doubleArray0[1] = (-1372.637618619536);
      doubleArray0[2] = (-1372.637618619536);
      // Undeclared exception!
      gaussian_Parametric0.value((-1372.637618619536), doubleArray0);
  }
}
