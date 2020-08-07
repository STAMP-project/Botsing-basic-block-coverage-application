/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon May 18 02:42:01 UTC 2020
 */

package org.apache.commons.math.ode.events;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class EventState_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.apache.commons.math.ode.events.EventState"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(EventState_ESTest_scaffolding.class.getClassLoader() ,
      "org.apache.commons.math.ode.DerivativeException",
      "org.apache.commons.math.MathException",
      "org.apache.commons.math.linear.ArrayRealVectorTest$RealVectorTestImpl",
      "org.apache.commons.math.ConvergenceException",
      "org.apache.commons.math.linear.RealMatrixImplTest$SetVisitor",
      "org.apache.commons.math.analysis.solvers.UnivariateRealSolver",
      "org.apache.commons.math.linear.RealMatrixImplTest$GetVisitor",
      "org.apache.commons.math.analysis.solvers.UnivariateRealSolverImpl",
      "org.apache.commons.math.util.OpenIntToDoubleHashMap",
      "org.apache.commons.math.ode.events.EventState$1",
      "org.apache.commons.math.analysis.UnivariateRealFunction",
      "org.apache.commons.math.linear.RealMatrix",
      "org.apache.commons.math.ode.events.EventException",
      "org.apache.commons.math.ode.sampling.DummyStepInterpolator",
      "org.apache.commons.math.ode.events.EventHandler",
      "org.apache.commons.math.linear.DefaultRealMatrixChangingVisitor",
      "org.apache.commons.math.ode.nonstiff.StepProblem",
      "org.apache.commons.math.linear.SparseRealVectorTest",
      "org.apache.commons.math.linear.ArrayRealVectorTest",
      "org.apache.commons.math.ode.TestProblem4$Bounce",
      "org.apache.commons.math.util.CompositeFormat",
      "org.apache.commons.math.ConvergingAlgorithm",
      "org.apache.commons.math.linear.SparseRealVector",
      "org.apache.commons.math.ode.sampling.StepInterpolator",
      "org.apache.commons.math.ode.FirstOrderDifferentialEquations",
      "org.apache.commons.math.MathRuntimeException",
      "org.apache.commons.math.linear.ArrayRealVector",
      "org.apache.commons.math.MathRuntimeException$1",
      "org.apache.commons.math.MathRuntimeException$2",
      "org.apache.commons.math.MathRuntimeException$3",
      "org.apache.commons.math.ode.AbstractIntegrator$EndTimeChecker",
      "org.apache.commons.math.MathRuntimeException$4",
      "org.apache.commons.math.MathRuntimeException$5",
      "org.apache.commons.math.MathRuntimeException$6",
      "org.apache.commons.math.MathRuntimeException$7",
      "org.apache.commons.math.MathRuntimeException$8",
      "org.apache.commons.math.MathRuntimeException$10",
      "org.apache.commons.math.MathRuntimeException$9",
      "org.apache.commons.math.linear.Array2DRowRealMatrixTest$SetVisitor",
      "org.apache.commons.math.linear.RealMatrixImpl",
      "org.apache.commons.math.linear.SparseRealVectorTest$SparseRealVectorTestImpl",
      "org.apache.commons.math.linear.DecompositionSolver",
      "org.apache.commons.math.linear.RealVectorFormat",
      "org.apache.commons.math.linear.AnyMatrix",
      "org.apache.commons.math.ode.sampling.NordsieckStepInterpolator",
      "org.apache.commons.math.linear.Array2DRowRealMatrix",
      "org.apache.commons.math.linear.RealMatrixPreservingVisitor",
      "org.apache.commons.math.MaxIterationsExceededException",
      "org.apache.commons.math.ode.sampling.AbstractStepInterpolator",
      "org.apache.commons.math.ode.sampling.DummyStepInterpolatorTest$BadStepInterpolator",
      "org.apache.commons.math.ode.events.EventState",
      "org.apache.commons.math.ode.TestProblem4$Stop",
      "org.apache.commons.math.linear.SparseRealMatrix",
      "org.apache.commons.math.linear.NonSquareMatrixException",
      "org.apache.commons.math.linear.BlockRealMatrixTest$GetVisitor",
      "org.apache.commons.math.linear.MatrixVisitorException",
      "org.apache.commons.math.FunctionEvaluationException",
      "org.apache.commons.math.linear.MatrixIndexException",
      "org.apache.commons.math.linear.AbstractRealMatrix",
      "org.apache.commons.math.linear.DefaultRealMatrixPreservingVisitor",
      "org.apache.commons.math.analysis.solvers.BrentSolver",
      "org.apache.commons.math.linear.BlockRealMatrix",
      "org.apache.commons.math.linear.OpenMapRealVector",
      "org.apache.commons.math.ConvergingAlgorithmImpl",
      "org.apache.commons.math.linear.InvalidMatrixException",
      "org.apache.commons.math.linear.RealVector",
      "org.apache.commons.math.linear.RealMatrixChangingVisitor",
      "org.apache.commons.math.linear.Array2DRowRealMatrixTest$GetVisitor",
      "org.apache.commons.math.linear.OpenMapRealMatrix",
      "org.apache.commons.math.linear.BlockRealMatrixTest$SetVisitor"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.apache.commons.math.ode.events.EventHandler", false, EventState_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.apache.commons.math.ode.sampling.StepInterpolator", false, EventState_ESTest_scaffolding.class.getClassLoader()));
  }
}
