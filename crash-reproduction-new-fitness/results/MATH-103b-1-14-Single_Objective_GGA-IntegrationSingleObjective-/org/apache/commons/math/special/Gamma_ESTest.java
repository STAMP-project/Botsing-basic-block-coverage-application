/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:49:35 UTC 2020
 */

package org.apache.commons.math.special;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.MaxIterationsExceededException;
import org.apache.commons.math.special.Gamma;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Gamma_ESTest extends Gamma_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Gamma.regularizedGammaQ((-2737.329266), (-2737.329266));
      Gamma.regularizedGammaP(Double.NaN, Double.NaN);
      double double0 = 0.0;
      Gamma.regularizedGammaP(0.0, 26.0355702768);
      double double1 = 2100.4261771975052;
      double double2 = (-548.7661828915893);
      int int0 = 913;
      Gamma.regularizedGammaP(2100.4261771975052, Double.NaN, (-548.7661828915893), 913);
      double double3 = 1893.87001781321;
      Gamma.regularizedGammaQ(1893.87001781321, 0.0, Double.NaN, (-3819));
      Gamma.regularizedGammaP(1.0, 0.0);
      try { 
        Gamma.regularizedGammaP(1110.12787, 1.0, (-2737.329266), 2221);
        fail("Expecting exception: MaxIterationsExceededException");
      
      } catch(MaxIterationsExceededException e) {
         //
         // Maximal number of iterations (2,221) exceeded
         //
         verifyException("org.apache.commons.math.special.Gamma", e);
      }
  }
}
