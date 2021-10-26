/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 14:46:38 UTC 2021
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
      double[] doubleArray0 = new double[0];
      levenbergMarquardtEstimator0.jacobian = doubleArray0;
      levenbergMarquardtEstimator0.setParRelativeTolerance((-1.0));
      levenbergMarquardtEstimator0.setCostRelativeTolerance((-1457.1479963415784));
      levenbergMarquardtEstimator0.getCostEvaluations();
      levenbergMarquardtEstimator0.incrementJacobianEvaluationsCounter();
      SimpleEstimationProblem simpleEstimationProblem0 = new SimpleEstimationProblem();
      EstimatedParameter estimatedParameter0 = new EstimatedParameter("t2", (-998.9323582822503));
      simpleEstimationProblem0.addParameter(estimatedParameter0);
      WeightedMeasurement weightedMeasurement0 = mock(WeightedMeasurement.class, new ViolatedAssumptionAnswer());
      simpleEstimationProblem0.addMeasurement(weightedMeasurement0);
      EstimatedParameter estimatedParameter1 = new EstimatedParameter((String) null, 2087.617542146943, true);
      simpleEstimationProblem0.addParameter(estimatedParameter1);
      // Undeclared exception!
      levenbergMarquardtEstimator0.getCovariances(simpleEstimationProblem0);
  }
}
