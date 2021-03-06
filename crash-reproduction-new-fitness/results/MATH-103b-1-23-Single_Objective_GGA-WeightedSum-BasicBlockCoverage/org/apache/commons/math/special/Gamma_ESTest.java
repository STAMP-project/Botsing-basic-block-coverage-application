/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:32:34 UTC 2020
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
      double double0 = 2934.45963;
      try { 
        Gamma.regularizedGammaP(2934.45963, 2934.45963, 0.0, 8);
        fail("Expecting exception: MaxIterationsExceededException");
      
      } catch(MaxIterationsExceededException e) {
         //
         // Maximal number of iterations (8) exceeded
         //
         verifyException("org.apache.commons.math.special.Gamma", e);
      }
  }
}
