/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon May 18 03:29:16 UTC 2020
 */

package org.apache.commons.math.analysis;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class UnivariateRealSolverUtils_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.apache.commons.math.analysis.UnivariateRealSolverUtils"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(UnivariateRealSolverUtils_ESTest_scaffolding.class.getClassLoader() ,
      "org.apache.commons.math.FunctionEvaluationException",
      "org.apache.commons.math.analysis.PolynomialSplineFunction",
      "org.apache.commons.math.analysis.UnivariateRealSolver",
      "org.apache.commons.math.analysis.Expm1Function",
      "org.apache.commons.math.analysis.PolynomialFunctionLagrangeForm",
      "org.apache.commons.math.analysis.UnivariateRealSolverUtils",
      "org.apache.commons.math.MathException",
      "org.apache.commons.math.analysis.MonitoredFunction",
      "org.apache.commons.math.DuplicateSampleAbscissaException",
      "org.apache.commons.math.analysis.UnivariateRealSolverImpl",
      "org.apache.commons.math.analysis.PolynomialFunction",
      "org.apache.commons.math.ConvergenceException",
      "org.apache.commons.math.analysis.Expm1Function$1",
      "org.apache.commons.math.analysis.BrentSolver",
      "org.apache.commons.math.analysis.QuinticFunction",
      "org.apache.commons.math.analysis.DifferentiableUnivariateRealFunction",
      "org.apache.commons.math.analysis.UnivariateRealFunction",
      "org.apache.commons.math.analysis.PolynomialFunctionNewtonForm",
      "org.apache.commons.math.ArgumentOutsideDomainException",
      "org.apache.commons.math.analysis.UnivariateRealSolverFactoryImpl",
      "org.apache.commons.math.analysis.SinFunction",
      "org.apache.commons.math.MaxIterationsExceededException",
      "org.apache.commons.math.analysis.UnivariateRealSolverFactory"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.apache.commons.math.analysis.UnivariateRealFunction", false, UnivariateRealSolverUtils_ESTest_scaffolding.class.getClassLoader()));
  }
}
