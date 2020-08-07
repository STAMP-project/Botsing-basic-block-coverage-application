/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:36:52 UTC 2020
 */

package org.apache.commons.math3.fraction;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.fraction.BigFraction;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BigFraction_ESTest extends BigFraction_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BigFraction bigFraction0 = new BigFraction((-1.2030229087793677E-8), 130);
      BigFraction bigFraction1 = new BigFraction((-1.2030229087793677E-8), 130);
      bigFraction1.getField();
      BigFraction bigFraction2 = new BigFraction(1265);
      bigFraction2.getNumerator();
      BigFraction bigFraction3 = new BigFraction((-1540.7657978842808), 2519);
      BigFraction bigFraction4 = new BigFraction((double) 1265, 629);
  }
}
