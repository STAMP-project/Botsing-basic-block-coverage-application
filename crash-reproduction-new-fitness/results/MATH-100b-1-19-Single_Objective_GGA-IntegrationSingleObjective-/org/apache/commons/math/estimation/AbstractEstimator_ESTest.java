/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:04:20 UTC 2020
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
      levenbergMarquardtEstimator0.setParRelativeTolerance((-1908.0));
      levenbergMarquardtEstimator0.setOrthoTolerance(0.0);
      double[] doubleArray0 = new double[10];
      doubleArray0[0] = (double) 0;
      doubleArray0[1] = 0.0;
      doubleArray0[0] = 0.0;
      doubleArray0[5] = (double) 72;
      doubleArray0[6] = 0.0;
      doubleArray0[7] = 310.00213;
      levenbergMarquardtEstimator0.jacobian = doubleArray0;
      SimpleEstimationProblem simpleEstimationProblem0 = new SimpleEstimationProblem();
      levenbergMarquardtEstimator0.initializeEstimate(simpleEstimationProblem0);
      WeightedMeasurement weightedMeasurement0 = mock(WeightedMeasurement.class, new ViolatedAssumptionAnswer());
      doReturn(0.0).when(weightedMeasurement0).getResidual();
      doReturn(0.0).when(weightedMeasurement0).getWeight();
      simpleEstimationProblem0.addMeasurement(weightedMeasurement0);
      levenbergMarquardtEstimator0.setParRelativeTolerance(0.0);
      double double0 = 1192.8427411687;
      WeightedMeasurement weightedMeasurement1 = mock(WeightedMeasurement.class, new ViolatedAssumptionAnswer());
      doReturn(0.0).when(weightedMeasurement1).getResidual();
      doReturn(0.0).when(weightedMeasurement1).getWeight();
      levenbergMarquardtEstimator0.updateResidualsAndCost();
      EstimatedParameter estimatedParameter0 = new EstimatedParameter("unable to compute covariances: singular problem", 72.0);
      EstimatedParameter estimatedParameter1 = new EstimatedParameter(estimatedParameter0);
      simpleEstimationProblem0.addParameter(estimatedParameter1);
      simpleEstimationProblem0.addMeasurement(weightedMeasurement1);
      levenbergMarquardtEstimator0.setParRelativeTolerance(1192.8427411687);
      levenbergMarquardtEstimator0.cost = 0.0;
      // Undeclared exception!
      levenbergMarquardtEstimator0.guessParametersErrors(simpleEstimationProblem0);
  }
}
