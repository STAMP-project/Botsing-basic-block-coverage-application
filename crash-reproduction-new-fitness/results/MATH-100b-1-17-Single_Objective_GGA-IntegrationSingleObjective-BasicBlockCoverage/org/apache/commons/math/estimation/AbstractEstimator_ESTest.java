/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:06:05 UTC 2020
 */

package org.apache.commons.math.estimation;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.estimation.EstimatedParameter;
import org.apache.commons.math.estimation.LevenbergMarquardtEstimator;
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
      LevenbergMarquardtEstimator levenbergMarquardtEstimator0 = new LevenbergMarquardtEstimator();
      levenbergMarquardtEstimator0.setMaxCostEval((-375));
      SimpleEstimationProblem simpleEstimationProblem0 = new SimpleEstimationProblem();
      levenbergMarquardtEstimator0.initializeEstimate(simpleEstimationProblem0);
      EstimatedParameter estimatedParameter0 = new EstimatedParameter("XCbI", 0.0);
      EstimatedParameter estimatedParameter1 = new EstimatedParameter(estimatedParameter0);
      WeightedMeasurement weightedMeasurement0 = mock(WeightedMeasurement.class, new ViolatedAssumptionAnswer());
      simpleEstimationProblem0.addMeasurement(weightedMeasurement0);
      levenbergMarquardtEstimator0.setMaxCostEval((-375));
      simpleEstimationProblem0.addParameter(estimatedParameter1);
      // Undeclared exception!
      levenbergMarquardtEstimator0.getCovariances(simpleEstimationProblem0);
  }
}
