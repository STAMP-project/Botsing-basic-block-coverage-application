/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Tue Mar 31 11:24:27 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class DirectSearchOptimizer_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.apache.commons.math.optimization.direct.DirectSearchOptimizer"; 
    org.evosuite.runtime.GuiSupport.initialize(); 
    org.evosuite.runtime.RuntimeSettings.maxNumberOfIterationsPerLoop = 10000; 
    org.evosuite.runtime.RuntimeSettings.mockSystemIn = true; 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
    try { initMocksToAvoidTimeoutsInTheTests(); } catch(ClassNotFoundException e) {} 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(DirectSearchOptimizer_ESTest_scaffolding.class.getClassLoader() ,
      "org.apache.commons.math.optimization.direct.DirectSearchOptimizer$1",
      "org.apache.commons.math.MathException",
      "org.apache.commons.math.optimization.OptimizationException",
      "org.apache.commons.math.optimization.RealConvergenceChecker",
      "org.apache.commons.math.optimization.SimpleScalarValueChecker",
      "org.apache.commons.math.ConvergenceException",
      "org.apache.commons.math.MaxEvaluationsExceededException",
      "org.apache.commons.math.MaxIterationsExceededException",
      "org.apache.commons.math.analysis.MultivariateRealFunction",
      "org.apache.commons.math.optimization.MultivariateRealOptimizer",
      "org.apache.commons.math.FunctionEvaluationException",
      "org.apache.commons.math.optimization.RealPointValuePair",
      "org.apache.commons.math.optimization.direct.MultiDirectional",
      "org.apache.commons.math.MathRuntimeException",
      "org.apache.commons.math.MathRuntimeException$1",
      "org.apache.commons.math.MathRuntimeException$2",
      "org.apache.commons.math.MathRuntimeException$3",
      "org.apache.commons.math.MathRuntimeException$4",
      "org.apache.commons.math.MathRuntimeException$5",
      "org.apache.commons.math.MathRuntimeException$6",
      "org.apache.commons.math.MathRuntimeException$7",
      "org.apache.commons.math.optimization.direct.DirectSearchOptimizer",
      "org.apache.commons.math.MathRuntimeException$8",
      "org.apache.commons.math.optimization.GoalType",
      "org.apache.commons.math.MathRuntimeException$10",
      "org.apache.commons.math.MathRuntimeException$9"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.apache.commons.math.analysis.MultivariateRealFunction", false, DirectSearchOptimizer_ESTest_scaffolding.class.getClassLoader()));
  }
}
