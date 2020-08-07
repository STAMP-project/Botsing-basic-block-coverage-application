/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:04:27 UTC 2020
 */

package org.apache.commons.math.estimation;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.estimation.EstimatedParameter;
import org.apache.commons.math.estimation.EstimationProblem;
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
      SimpleEstimationProblem simpleEstimationProblem0 = new SimpleEstimationProblem();
      EstimationProblem estimationProblem0 = mock(EstimationProblem.class, new ViolatedAssumptionAnswer());
      EstimationProblem estimationProblem1 = mock(EstimationProblem.class, new ViolatedAssumptionAnswer());
      LevenbergMarquardtEstimator levenbergMarquardtEstimator0 = new LevenbergMarquardtEstimator();
      double[] doubleArray0 = new double[9];
      doubleArray0[0] = 425.9566768332456;
      EstimatedParameter estimatedParameter0 = new EstimatedParameter("unable to compute covariances: singular problem", 0.0, true);
      EstimatedParameter estimatedParameter1 = new EstimatedParameter(estimatedParameter0);
      simpleEstimationProblem0.addParameter(estimatedParameter1);
      simpleEstimationProblem0.addParameter(estimatedParameter0);
      doubleArray0[7] = 0.4529561723560528;
      levenbergMarquardtEstimator0.jacobian = doubleArray0;
      levenbergMarquardtEstimator0.incrementJacobianEvaluationsCounter();
      WeightedMeasurement weightedMeasurement0 = mock(WeightedMeasurement.class, new ViolatedAssumptionAnswer());
      doReturn(0.0).when(weightedMeasurement0).getWeight();
      simpleEstimationProblem0.addMeasurement(weightedMeasurement0);
      levenbergMarquardtEstimator0.setCostRelativeTolerance(3192.35957);
      levenbergMarquardtEstimator0.initializeEstimate(simpleEstimationProblem0);
      // Undeclared exception!
      levenbergMarquardtEstimator0.getCovariances(simpleEstimationProblem0);
  }
}
