/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:50:35 UTC 2020
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
      BigFraction bigFraction0 = BigFraction.FOUR_FIFTHS;
      double double0 = (-212.88472931);
      int int0 = 3748;
      BigFraction bigFraction1 = new BigFraction((-212.88472931), (-1628.86998766469), 3748);
  }
}
