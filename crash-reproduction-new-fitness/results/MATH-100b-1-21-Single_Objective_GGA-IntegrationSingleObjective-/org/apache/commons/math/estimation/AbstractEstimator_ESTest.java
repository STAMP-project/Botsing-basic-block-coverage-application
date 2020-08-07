/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:04:36 UTC 2020
 */

package org.apache.commons.math.estimation;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.estimation.EstimatedParameter;
import org.apache.commons.math.estimation.GaussNewtonEstimator;
import org.apache.commons.math.estimation.LevenbergMarquardtEstimatorTest;
import org.apache.commons.math.estimation.SimpleEstimationProblem;
import org.apache.commons.math.estimation.WeightedMeasurement;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractEstimator_ESTest extends AbstractEstimator_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      SimpleEstimationProblem simpleEstimationProblem0 = new SimpleEstimationProblem();
      EstimatedParameter estimatedParameter0 = new EstimatedParameter("maximal number of evaluations exceeded ({0})", (-1247.8584692776087));
      estimatedParameter0.setBound(true);
      simpleEstimationProblem0.addParameter(estimatedParameter0);
      WeightedMeasurement weightedMeasurement0 = mock(WeightedMeasurement.class, new ViolatedAssumptionAnswer());
      doReturn(0.0).when(weightedMeasurement0).getWeight();
      simpleEstimationProblem0.addMeasurement(weightedMeasurement0);
      int int0 = (-13);
      double double0 = 892.638;
      GaussNewtonEstimator gaussNewtonEstimator0 = new GaussNewtonEstimator((-13), 892.638, (-13));
      gaussNewtonEstimator0.initializeEstimate(simpleEstimationProblem0);
      LevenbergMarquardtEstimatorTest levenbergMarquardtEstimatorTest0 = new LevenbergMarquardtEstimatorTest("");
      simpleEstimationProblem0.addParameter(estimatedParameter0);
      // Undeclared exception!
      gaussNewtonEstimator0.getCovariances(simpleEstimationProblem0);
  }
}
