/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:49:19 UTC 2020
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
      double double0 = (-146.4907699341);
      Gamma.regularizedGammaP((-146.4907699341), 1.0, (-146.4907699341), 5);
      double double1 = 0.0;
      Gamma.regularizedGammaP(4.652362892704858E-5, 0.0, 1504.650547873, 5);
      Gamma.regularizedGammaP((-335.5874), 1504.650547873);
      double double2 = 3.6899182659531625E-6;
      Gamma.regularizedGammaQ(3.6899182659531625E-6, 0.0);
      double double3 = 844.5915222292418;
      int int0 = 0;
      try { 
        Gamma.regularizedGammaQ(1504.650547873, 844.5915222292418, 1504.650547873, 0);
        fail("Expecting exception: MaxIterationsExceededException");
      
      } catch(MaxIterationsExceededException e) {
         //
         // Maximal number of iterations (0) exceeded
         //
         verifyException("org.apache.commons.math.special.Gamma", e);
      }
  }
}
