/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:51:05 UTC 2020
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
      double double0 = (-0.7853981633974483);
      double[] doubleArray0 = new double[3];
      doubleArray0[0] = (-0.7853981633974483);
      doubleArray0[1] = (-0.7853981633974483);
      doubleArray0[2] = (-0.7853981633974483);
      // Undeclared exception!
      gaussian_Parametric0.value((-0.7853981633974483), doubleArray0);
  }
}
