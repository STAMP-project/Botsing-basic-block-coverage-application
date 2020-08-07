/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 22:11:45 UTC 2020
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
      PoissonDistributionImpl poissonDistributionImpl0 = new PoissonDistributionImpl(0.5);
      int int0 = 0;
      int int1 = 0;
      poissonDistributionImpl0.cumulativeProbability(0, 0);
      PoissonDistributionImpl poissonDistributionImpl1 = new PoissonDistributionImpl((-3224.2667587083647));
  }
}
