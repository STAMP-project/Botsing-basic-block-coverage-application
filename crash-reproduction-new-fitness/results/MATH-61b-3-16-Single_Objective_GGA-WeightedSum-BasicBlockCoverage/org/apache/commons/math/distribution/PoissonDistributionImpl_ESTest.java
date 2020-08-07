/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:36:54 UTC 2020
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
      PoissonDistributionImpl poissonDistributionImpl0 = new PoissonDistributionImpl(0.7937005259840998, 149);
      poissonDistributionImpl0.getMean();
      poissonDistributionImpl0.reseedRandomGenerator((-6206L));
      PoissonDistributionImpl poissonDistributionImpl1 = new PoissonDistributionImpl(6.283185307179586);
      poissonDistributionImpl0.getDomainUpperBound(6.283185307179586);
      poissonDistributionImpl0.cumulativeProbability((double) (-6206L));
      poissonDistributionImpl1.normalApproximateProbability(518);
      PoissonDistributionImpl poissonDistributionImpl2 = new PoissonDistributionImpl(0.7937005259840998);
      PoissonDistributionImpl poissonDistributionImpl3 = new PoissonDistributionImpl(0.0);
  }
}
