/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 03:15:11 UTC 2020
 */

package org.apache.commons.math.distribution;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.distribution.PoissonDistributionImpl;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PoissonDistributionImpl_ESTest extends PoissonDistributionImpl_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      int int0 = 6125;
      PoissonDistributionImpl poissonDistributionImpl0 = new PoissonDistributionImpl((-1007.00072708648), 1.0E-12, 6125);
  }
}
