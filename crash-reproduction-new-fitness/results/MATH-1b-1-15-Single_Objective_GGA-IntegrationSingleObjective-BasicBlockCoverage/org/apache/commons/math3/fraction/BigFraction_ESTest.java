/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:48:07 UTC 2020
 */

package org.apache.commons.math3.fraction;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.math.BigInteger;
import org.apache.commons.math3.fraction.BigFraction;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BigFraction_ESTest extends BigFraction_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      int int0 = (-2997);
      BigFraction bigFraction0 = new BigFraction(1415.0, 1415.0, (-2997));
      bigFraction0.getNumerator();
      BigFraction.getReducedFraction(651, (-2997));
      BigInteger bigInteger0 = bigFraction0.getDenominator();
      BigFraction bigFraction1 = new BigFraction((-2997));
      int int1 = 1592;
      BigFraction bigFraction2 = new BigFraction(6.283185307179586, 1592);
      bigFraction2.divide(bigInteger0);
      bigFraction2.divide(bigInteger0);
      double double0 = 0.0;
      BigFraction bigFraction3 = new BigFraction(0.0, 295);
  }
}
