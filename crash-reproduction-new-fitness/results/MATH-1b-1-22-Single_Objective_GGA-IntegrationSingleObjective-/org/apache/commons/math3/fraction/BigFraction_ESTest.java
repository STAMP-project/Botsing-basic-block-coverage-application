/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:14:54 UTC 2020
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
      BigInteger bigInteger0 = BigInteger.ONE;
      BigFraction bigFraction0 = new BigFraction(bigInteger0);
      BigFraction bigFraction1 = bigFraction0.multiply((-1));
      bigFraction0.compareTo(bigFraction1);
      BigFraction bigFraction2 = bigFraction1.add(bigFraction1);
      BigFraction bigFraction3 = bigFraction0.subtract(4503599627370496L);
      bigFraction3.subtract(bigFraction2);
      bigFraction3.getNumerator();
      double double0 = 4219.0;
      BigFraction bigFraction4 = new BigFraction(4219.0);
      BigFraction bigFraction5 = new BigFraction((double) 1, (-1987));
  }
}
