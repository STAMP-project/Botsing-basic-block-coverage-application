/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:15:26 UTC 2020
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
      BigFraction bigFraction0 = BigFraction.FOUR_FIFTHS;
      BigFraction bigFraction1 = new BigFraction(0.0);
      BigFraction bigFraction2 = BigFraction.getReducedFraction(1178, 1178);
      bigFraction0.compareTo(bigFraction1);
      bigFraction2.hashCode();
      BigFraction bigFraction3 = bigFraction0.pow((long) 1178);
      BigFraction bigFraction4 = new BigFraction(1178, (-2644281811660520851L));
      bigFraction3.getField();
      BigFraction bigFraction5 = bigFraction3.reduce();
      bigFraction1.bigDecimalValue(2);
      BigFraction bigFraction6 = bigFraction5.subtract(bigFraction2);
      BigFraction bigFraction7 = new BigFraction(0.0, 1, 1);
      bigFraction2.equals(bigFraction6);
      BigFraction bigFraction8 = new BigFraction((long) 1178);
      BigFraction bigFraction9 = BigFraction.ONE_HALF;
      BigFraction bigFraction10 = bigFraction8.multiply(bigFraction9);
      BigInteger bigInteger0 = BigInteger.TEN;
      bigFraction9.divide(bigInteger0);
      bigFraction9.getField();
      bigFraction10.bigDecimalValue(1);
      BigFraction bigFraction11 = bigFraction10.subtract((long) 1178);
      bigFraction3.floatValue();
      bigFraction1.abs();
      bigFraction11.divide(2);
      bigFraction2.multiply(383L);
      bigFraction1.intValue();
      BigFraction bigFraction12 = new BigFraction(0.0, 2);
  }
}
