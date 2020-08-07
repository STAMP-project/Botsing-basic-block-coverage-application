/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 11:39:09 UTC 2020
 */

package org.apache.commons.math.estimation;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.estimation.EstimatedParameter;
import org.apache.commons.math.estimation.GaussNewtonEstimator;
import org.apache.commons.math.estimation.SimpleEstimationProblem;
import org.apache.commons.math.estimation.WeightedMeasurement;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractEstimator_ESTest extends AbstractEstimator_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      GaussNewtonEstimator gaussNewtonEstimator0 = new GaussNewtonEstimator(2437, 2437, 1.7853723965756338);
      SimpleEstimationProblem simpleEstimationProblem0 = new SimpleEstimationProblem();
      gaussNewtonEstimator0.initializeEstimate(simpleEstimationProblem0);
      gaussNewtonEstimator0.incrementJacobianEvaluationsCounter();
      simpleEstimationProblem0.addMeasurement((WeightedMeasurement) null);
      EstimatedParameter estimatedParameter0 = new EstimatedParameter("cost relative tolerance is too small ({0}), no further reduction in the sum of squares is possible", 926.4620985);
      simpleEstimationProblem0.addParameter(estimatedParameter0);
      // Undeclared exception!
      gaussNewtonEstimator0.getCovariances(simpleEstimationProblem0);
  }
}
