/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sat May 16 11:41:27 UTC 2020
 */

package org.apache.commons.math.estimation;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class AbstractEstimator_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.apache.commons.math.estimation.AbstractEstimator"; 
    org.evosuite.runtime.GuiSupport.initialize(); 
    org.evosuite.runtime.RuntimeSettings.maxNumberOfIterationsPerLoop = 10000; 
    org.evosuite.runtime.RuntimeSettings.mockSystemIn = true; 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
  } 

  @Before 
  public void initTestCase(){ 
    threadStopper.storeCurrentThreads();
    threadStopper.startRecordingTime();
    org.evosuite.runtime.GuiSupport.setHeadless(); 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
    org.evosuite.runtime.agent.InstrumentingAgent.activate(); 
  } 

  @After 
  public void doneWithTestCase(){ 
    threadStopper.killAndJoinClientThreads();
    org.evosuite.runtime.agent.InstrumentingAgent.deactivate(); 
    org.evosuite.runtime.GuiSupport.restoreHeadlessMode(); 
  } 


  private static void initializeClasses() {
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(AbstractEstimator_ESTest_scaffolding.class.getClassLoader() ,
      "org.apache.commons.math.estimation.MinpackTest$FreudensteinRothFunction",
      "org.apache.commons.math.estimation.EstimatedParameter",
      "org.apache.commons.math.estimation.MinpackTest$MinpackFunction",
      "org.apache.commons.math.estimation.MinpackTest$LinearRank1ZeroColsAndRowsFunction",
      "org.apache.commons.math.estimation.MinpackTest$Box3DimensionalFunction",
      "org.apache.commons.math.estimation.EstimationException",
      "org.apache.commons.math.estimation.MinpackTest$KowalikOsborneFunction",
      "org.apache.commons.math.estimation.MinpackTest$PowellSingularFunction",
      "org.apache.commons.math.linear.RealMatrixImpl",
      "org.apache.commons.math.MathException",
      "org.apache.commons.math.estimation.MinpackTest$HelicalValleyFunction",
      "org.apache.commons.math.estimation.GaussNewtonEstimatorTest$LinearProblem",
      "org.apache.commons.math.estimation.GaussNewtonEstimatorTest$Circle$PointModel",
      "org.apache.commons.math.estimation.Estimator",
      "org.apache.commons.math.estimation.LevenbergMarquardtEstimator",
      "org.apache.commons.math.estimation.LevenbergMarquardtEstimatorTest$QuadraticProblem$LocalMeasurement",
      "org.apache.commons.math.linear.MatrixUtils",
      "org.apache.commons.math.estimation.WeightedMeasurementTest$MyMeasurement",
      "org.apache.commons.math.linear.RealMatrix",
      "org.apache.commons.math.estimation.GaussNewtonEstimatorTest$Circle",
      "org.apache.commons.math.estimation.MinpackTest$LinearRank1Function",
      "org.apache.commons.math.estimation.EstimationProblem",
      "org.apache.commons.math.estimation.MinpackTest$JennrichSampsonFunction",
      "org.apache.commons.math.estimation.MinpackTest$MinpackFunction$MinpackMeasurement",
      "org.apache.commons.math.estimation.MinpackTest$LinearFullRankFunction",
      "org.apache.commons.math.linear.MatrixIndexException",
      "org.apache.commons.math.estimation.MinpackTest$ChebyquadFunction",
      "org.apache.commons.math.estimation.WeightedMeasurement",
      "org.apache.commons.math.estimation.SimpleEstimationProblem",
      "org.apache.commons.math.estimation.MinpackTest$BrownAlmostLinearFunction",
      "org.apache.commons.math.estimation.MinpackTest$RosenbrockFunction",
      "org.apache.commons.math.estimation.MinpackTest$WatsonFunction",
      "org.apache.commons.math.linear.BigMatrix",
      "org.apache.commons.math.estimation.MinpackTest$Osborne1Function",
      "org.apache.commons.math.estimation.MinpackTest$Osborne2Function",
      "org.apache.commons.math.estimation.LevenbergMarquardtEstimatorTest$Circle",
      "org.apache.commons.math.estimation.MinpackTest$MeyerFunction",
      "org.apache.commons.math.estimation.GaussNewtonEstimatorTest$LinearMeasurement",
      "org.apache.commons.math.estimation.AbstractEstimator",
      "org.apache.commons.math.linear.InvalidMatrixException",
      "org.apache.commons.math.estimation.LevenbergMarquardtEstimatorTest$QuadraticProblem",
      "org.apache.commons.math.estimation.LevenbergMarquardtEstimatorTest$Circle$PointModel",
      "org.apache.commons.math.estimation.LevenbergMarquardtEstimatorTest",
      "org.apache.commons.math.estimation.MinpackTest$BardFunction",
      "org.apache.commons.math.estimation.MinpackTest$BrownDennisFunction",
      "org.apache.commons.math.estimation.LevenbergMarquardtEstimatorTest$LinearMeasurement",
      "org.apache.commons.math.estimation.GaussNewtonEstimator",
      "org.apache.commons.math.estimation.LevenbergMarquardtEstimatorTest$LinearProblem"
    );
  } 
}
