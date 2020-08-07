/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:37:53 UTC 2020
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
      PoissonDistributionImpl poissonDistributionImpl0 = new PoissonDistributionImpl(511.892);
      poissonDistributionImpl0.getMean();
      double double0 = 1160.26952212683;
      poissonDistributionImpl0.cumulativeProbability((-1742.84), 1160.26952212683);
      poissonDistributionImpl0.getDomainUpperBound(2174.5379);
      poissonDistributionImpl0.probability(Integer.MAX_VALUE);
      PoissonDistributionImpl poissonDistributionImpl1 = new PoissonDistributionImpl(2385.3, 674);
      poissonDistributionImpl1.cumulativeProbability((-2137573175), Integer.MAX_VALUE);
      PoissonDistributionImpl poissonDistributionImpl2 = new PoissonDistributionImpl(1549.0721979);
      poissonDistributionImpl1.reseedRandomGenerator(1036);
      poissonDistributionImpl2.getMean();
      PoissonDistributionImpl poissonDistributionImpl3 = new PoissonDistributionImpl(0.13333332538604736, 32767);
      poissonDistributionImpl3.probability((-198.8159049));
      PoissonDistributionImpl poissonDistributionImpl4 = new PoissonDistributionImpl((-1413.0));
  }
}
