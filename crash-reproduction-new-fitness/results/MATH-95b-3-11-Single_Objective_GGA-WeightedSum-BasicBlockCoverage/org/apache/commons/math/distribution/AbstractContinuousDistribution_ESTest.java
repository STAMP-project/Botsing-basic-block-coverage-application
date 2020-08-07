/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 22:26:36 UTC 2020
 */

package org.apache.commons.math.distribution;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.distribution.BetaDistributionImpl;
import org.apache.commons.math.distribution.FDistributionImpl;
import org.apache.commons.math.distribution.TDistributionImpl;
import org.apache.commons.math.distribution.WeibullDistributionImpl;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractContinuousDistribution_ESTest extends AbstractContinuousDistribution_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = 234.1431;
      double double1 = 2955.613082942957;
      TDistributionImpl tDistributionImpl0 = new TDistributionImpl(234.1431);
      FDistributionImpl fDistributionImpl0 = new FDistributionImpl(2955.613082942957, 1.580887032249125E-4);
      double double2 = 592.291129559563;
      fDistributionImpl0.setNumeratorDegreesOfFreedom(234.1431);
      Double double3 = new Double(1.580887032249125E-4);
      WeibullDistributionImpl weibullDistributionImpl0 = new WeibullDistributionImpl(2955.613082942957, 1.7896169203653245);
      fDistributionImpl0.setNumeratorDegreesOfFreedom(0.8520017701534994);
      Double.max(3.6899182659531625E-6, 0.524136487738816);
      BetaDistributionImpl betaDistributionImpl0 = new BetaDistributionImpl(2955.613082942957, 234.1431);
      double double4 = 1.0E-6;
      // Undeclared exception!
      fDistributionImpl0.inverseCumulativeProbability(1.0E-6);
  }
}
