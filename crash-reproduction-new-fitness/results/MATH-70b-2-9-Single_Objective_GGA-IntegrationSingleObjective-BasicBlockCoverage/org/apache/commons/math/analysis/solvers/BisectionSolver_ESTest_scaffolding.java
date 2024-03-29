/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sat May 16 12:41:14 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class BisectionSolver_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.apache.commons.math.analysis.solvers.BisectionSolver"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(BisectionSolver_ESTest_scaffolding.class.getClassLoader() ,
      "org.apache.commons.math.analysis.solvers.UnivariateRealSolverUtils",
      "org.apache.commons.math.MathException",
      "org.apache.commons.math.ConvergenceException",
      "org.apache.commons.math.analysis.solvers.UnivariateRealSolver",
      "org.apache.commons.math.analysis.solvers.BisectionSolver",
      "org.apache.commons.math.analysis.solvers.UnivariateRealSolverImpl",
      "org.apache.commons.math.analysis.DifferentiableUnivariateRealFunction",
      "org.apache.commons.math.analysis.UnivariateRealFunction",
      "org.apache.commons.math.analysis.SinFunction",
      "org.apache.commons.math.analysis.MultivariateRealFunction",
      "org.apache.commons.math.analysis.ComposableFunction$2",
      "org.apache.commons.math.analysis.ComposableFunction$1",
      "org.apache.commons.math.analysis.ComposableFunction$4",
      "org.apache.commons.math.analysis.ComposableFunction$3",
      "org.apache.commons.math.analysis.ComposableFunction$6",
      "org.apache.commons.math.analysis.ComposableFunction$5",
      "org.apache.commons.math.ConvergingAlgorithm",
      "org.apache.commons.math.analysis.ComposableFunction$17",
      "org.apache.commons.math.analysis.ComposableFunction$16",
      "org.apache.commons.math.analysis.ComposableFunction$19",
      "org.apache.commons.math.analysis.ComposableFunction$18",
      "org.apache.commons.math.analysis.ComposableFunction$13",
      "org.apache.commons.math.MathRuntimeException",
      "org.apache.commons.math.analysis.ComposableFunction$12",
      "org.apache.commons.math.analysis.ComposableFunction$15",
      "org.apache.commons.math.MathRuntimeException$1",
      "org.apache.commons.math.analysis.ComposableFunction$14",
      "org.apache.commons.math.MathRuntimeException$2",
      "org.apache.commons.math.MathRuntimeException$3",
      "org.apache.commons.math.MathRuntimeException$4",
      "org.apache.commons.math.analysis.ComposableFunction$11",
      "org.apache.commons.math.MathRuntimeException$5",
      "org.apache.commons.math.analysis.BivariateRealFunction",
      "org.apache.commons.math.analysis.ComposableFunction$10",
      "org.apache.commons.math.MathRuntimeException$6",
      "org.apache.commons.math.MathRuntimeException$7",
      "org.apache.commons.math.MathRuntimeException$8",
      "org.apache.commons.math.MathRuntimeException$10",
      "org.apache.commons.math.MathRuntimeException$9",
      "org.apache.commons.math.analysis.ComposableFunction",
      "org.apache.commons.math.analysis.ComposableFunction$28",
      "org.apache.commons.math.analysis.ComposableFunction$27",
      "org.apache.commons.math.analysis.ComposableFunction$29",
      "org.apache.commons.math.analysis.ComposableFunction$24",
      "org.apache.commons.math.analysis.ComposableFunction$23",
      "org.apache.commons.math.analysis.ComposableFunction$26",
      "org.apache.commons.math.analysis.ComposableFunction$25",
      "org.apache.commons.math.MaxIterationsExceededException",
      "org.apache.commons.math.analysis.ComposableFunction$8",
      "org.apache.commons.math.analysis.ComposableFunction$20",
      "org.apache.commons.math.analysis.ComposableFunction$7",
      "org.apache.commons.math.analysis.ComposableFunction$22",
      "org.apache.commons.math.analysis.ComposableFunction$9",
      "org.apache.commons.math.analysis.ComposableFunction$21",
      "org.apache.commons.math.FunctionEvaluationException",
      "org.apache.commons.math.analysis.Expm1Function",
      "org.apache.commons.math.analysis.MonitoredFunction",
      "org.apache.commons.math.analysis.QuinticFunction",
      "org.apache.commons.math.ConvergingAlgorithmImpl",
      "org.apache.commons.math.analysis.ComposableFunction$35",
      "org.apache.commons.math.analysis.ComposableFunction$34",
      "org.apache.commons.math.analysis.ComposableFunction$36",
      "org.apache.commons.math.analysis.ComposableFunction$31",
      "org.apache.commons.math.analysis.ComposableFunction$30",
      "org.apache.commons.math.analysis.ComposableFunction$33",
      "org.apache.commons.math.analysis.ComposableFunction$32"
    );
  } 
}
