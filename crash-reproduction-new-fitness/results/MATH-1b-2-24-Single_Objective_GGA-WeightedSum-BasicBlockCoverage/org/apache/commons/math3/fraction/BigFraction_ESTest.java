/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:36:44 UTC 2020
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
      BigFraction bigFraction0 = new BigFraction((-1438.4326257131108), (-1));
      bigFraction0.bigDecimalValue();
      int int0 = 979;
      BigFraction bigFraction1 = bigFraction0.add(979);
      BigFraction bigFraction2 = bigFraction0.add(979);
      bigFraction1.getNumeratorAsLong();
      bigFraction0.subtract((-361));
      bigFraction2.floatValue();
      BigInteger bigInteger0 = BigInteger.TEN;
      bigFraction2.subtract(bigInteger0);
      int int1 = 100;
      BigFraction bigFraction3 = new BigFraction((-460.0F), 100);
  }
}
