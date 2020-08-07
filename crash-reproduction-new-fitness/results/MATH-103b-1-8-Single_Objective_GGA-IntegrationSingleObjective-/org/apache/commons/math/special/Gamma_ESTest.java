/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:46:36 UTC 2020
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
      double double0 = 1.0E-8;
      double double1 = 1467.605043777714;
      double double2 = (-34.096248526729);
      int int0 = (-289);
      try { 
        Gamma.regularizedGammaQ(1.0E-8, 1467.605043777714, (-34.096248526729), (-289));
        fail("Expecting exception: MaxIterationsExceededException");
      
      } catch(MaxIterationsExceededException e) {
         //
         // Maximal number of iterations (-289) exceeded
         //
         verifyException("org.apache.commons.math.special.Gamma", e);
      }
  }
}
