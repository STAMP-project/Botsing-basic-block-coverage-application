/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:10:43 UTC 2020
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
      BigFraction bigFraction0 = BigFraction.TWO;
      bigFraction0.shortValue();
      BigFraction bigFraction1 = BigFraction.getReducedFraction((short)2, 2525);
      bigFraction1.getDenominatorAsInt();
      bigFraction0.hashCode();
      BigFraction bigFraction2 = new BigFraction((double) (short)2, (-3362));
  }
}
