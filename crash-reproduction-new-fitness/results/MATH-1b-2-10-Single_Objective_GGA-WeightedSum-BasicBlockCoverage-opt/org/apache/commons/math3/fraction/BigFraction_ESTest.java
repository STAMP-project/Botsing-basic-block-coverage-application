/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:46:05 UTC 2021
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
      int int0 = (-640);
      BigFraction bigFraction0 = new BigFraction(3776.130802127772, (-640));
      int int1 = 454;
      BigFraction.getReducedFraction(454, 3023);
      bigFraction0.divide((-640));
      BigFraction bigFraction1 = new BigFraction((double) (-640), 454);
  }
}
