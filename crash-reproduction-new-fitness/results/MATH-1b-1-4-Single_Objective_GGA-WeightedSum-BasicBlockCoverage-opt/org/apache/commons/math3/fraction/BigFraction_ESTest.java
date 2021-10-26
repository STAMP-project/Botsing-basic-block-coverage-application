/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:45:52 UTC 2021
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
      byte[] byteArray0 = new byte[3];
      byteArray0[0] = (byte)0;
      byteArray0[1] = (byte)0;
      byte byte0 = (byte)1;
      byteArray0[2] = (byte)1;
      BigInteger bigInteger0 = new BigInteger(byteArray0);
      BigFraction bigFraction0 = new BigFraction(bigInteger0);
      bigFraction0.multiply((long) (byte)0);
      int int0 = 85;
      BigFraction bigFraction1 = new BigFraction((double) (byte)0, 85);
  }
}
