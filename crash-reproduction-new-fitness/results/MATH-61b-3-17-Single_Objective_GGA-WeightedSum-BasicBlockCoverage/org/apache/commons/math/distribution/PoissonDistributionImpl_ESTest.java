/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:37:11 UTC 2020
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
      PoissonDistributionImpl poissonDistributionImpl0 = new PoissonDistributionImpl(4.5057572876067935, 4.5057572876067935);
      PoissonDistributionImpl poissonDistributionImpl1 = new PoissonDistributionImpl(4.5057572876067935);
      poissonDistributionImpl0.getMean();
      double double0 = (-4165.425488197);
      PoissonDistributionImpl poissonDistributionImpl2 = new PoissonDistributionImpl((-4165.425488197));
  }
}
