/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:38:25 UTC 2020
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
      double double0 = 1325.4109;
      PoissonDistributionImpl poissonDistributionImpl0 = new PoissonDistributionImpl(1325.4109, 1325.4109);
      int int0 = 1;
      poissonDistributionImpl0.probability(1);
      long long0 = 668L;
      poissonDistributionImpl0.reseedRandomGenerator(668L);
      PoissonDistributionImpl poissonDistributionImpl1 = new PoissonDistributionImpl(6.283185307179586, 0.0);
      poissonDistributionImpl1.probability(1);
      PoissonDistributionImpl poissonDistributionImpl2 = new PoissonDistributionImpl((-81.384465), 0.011733488733866947, 1);
  }
}
