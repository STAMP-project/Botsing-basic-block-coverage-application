/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:47:56 UTC 2020
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
      BigInteger bigInteger0 = BigInteger.TEN;
      BigFraction bigFraction0 = new BigFraction(bigInteger0, bigInteger0);
      bigFraction0.pow((-2369L));
      BigFraction bigFraction1 = new BigFraction(bigInteger0, bigInteger0);
      BigFraction bigFraction2 = new BigFraction(bigInteger0);
      bigFraction1.add((-3561L));
      int int0 = 724;
      BigFraction bigFraction3 = new BigFraction((-1141.4468201455811), 724);
      bigFraction2.getDenominator();
      int int1 = 1466;
      bigFraction3.divide(1466);
      int int2 = (-162);
      BigFraction bigFraction4 = new BigFraction((double) 724, (-162));
  }
}
