/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:46:05 UTC 2021
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
      BigFraction bigFraction0 = new BigFraction(bigInteger0);
      bigFraction0.divide(bigInteger0);
      BigFraction bigFraction1 = new BigFraction(0.0, 0);
  }
}
