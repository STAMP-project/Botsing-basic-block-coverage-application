/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Thu May 14 22:16:52 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class MultiDirectional_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.apache.commons.math.optimization.direct.MultiDirectional"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(MultiDirectional_ESTest_scaffolding.class.getClassLoader() ,
      "org.apache.commons.math.analysis.MultivariateVectorialFunction",
      "org.apache.commons.math.optimization.general.MinpackTest$WatsonFunction",
      "org.apache.commons.math.optimization.general.MinpackTest$LinearRank1Function",
      "org.apache.commons.math.MathException",
      "org.apache.commons.math.optimization.general.MinpackTest$KowalikOsborneFunction",
      "org.apache.commons.math.optimization.RealConvergenceChecker",
      "org.apache.commons.math.optimization.general.MinpackTest$LinearFullRankFunction",
      "org.apache.commons.math.optimization.SimpleScalarValueChecker",
      "org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizerTest$Circle",
      "org.apache.commons.math.linear.ArrayRealVectorTest$RealVectorTestImpl",
      "org.apache.commons.math.optimization.general.MinpackTest$Osborne2Function",
      "org.apache.commons.math.ConvergenceException",
      "org.apache.commons.math.linear.RealMatrixImplTest$SetVisitor",
      "org.apache.commons.math.optimization.general.MinpackTest$RosenbrockFunction",
      "org.apache.commons.math.linear.RealMatrixImplTest$GetVisitor",
      "org.apache.commons.math.util.OpenIntToDoubleHashMap",
      "org.apache.commons.math.optimization.MultiStartDifferentiableMultivariateVectorialOptimizerTest$LinearProblem",
      "org.apache.commons.math.linear.RealMatrix",
      "org.apache.commons.math.linear.DefaultRealMatrixChangingVisitor",
      "org.apache.commons.math.analysis.MultivariateRealFunction",
      "org.apache.commons.math.optimization.general.GaussNewtonOptimizerTest$Circle",
      "org.apache.commons.math.optimization.MultivariateRealOptimizer",
      "org.apache.commons.math.optimization.MultiStartDifferentiableMultivariateRealOptimizerTest$Circle",
      "org.apache.commons.math.linear.SparseRealVectorTest",
      "org.apache.commons.math.analysis.DifferentiableMultivariateRealFunction",
      "org.apache.commons.math.linear.ArrayRealVectorTest",
      "org.apache.commons.math.util.CompositeFormat",
      "org.apache.commons.math.optimization.general.MinpackTest$MinpackFunction",
      "org.apache.commons.math.linear.SparseRealVector",
      "org.apache.commons.math.optimization.general.MinpackTest$FreudensteinRothFunction",
      "org.apache.commons.math.optimization.general.MinpackTest$HelicalValleyFunction",
      "org.apache.commons.math.MathRuntimeException",
      "org.apache.commons.math.linear.ArrayRealVector",
      "org.apache.commons.math.optimization.general.MinpackTest$LinearRank1ZeroColsAndRowsFunction",
      "org.apache.commons.math.optimization.general.MinpackTest$ChebyquadFunction",
      "org.apache.commons.math.optimization.SimpleRealPointChecker",
      "org.apache.commons.math.optimization.general.MinpackTest$BardFunction",
      "org.apache.commons.math.optimization.MultiStartMultivariateRealOptimizerTest$Rosenbrock",
      "org.apache.commons.math.optimization.general.MinpackTest$PowellSingularFunction",
      "org.apache.commons.math.linear.Array2DRowRealMatrixTest$SetVisitor",
      "org.apache.commons.math.optimization.general.MinpackTest$Osborne1Function",
      "org.apache.commons.math.linear.RealMatrixImpl",
      "org.apache.commons.math.linear.SparseRealVectorTest$SparseRealVectorTestImpl",
      "org.apache.commons.math.optimization.OptimizationException",
      "org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizerTest$QuadraticProblem",
      "org.apache.commons.math.linear.DecompositionSolver",
      "org.apache.commons.math.linear.RealVectorFormat",
      "org.apache.commons.math.MaxEvaluationsExceededException",
      "org.apache.commons.math.linear.AnyMatrix",
      "org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizerTest$LinearProblem",
      "org.apache.commons.math.optimization.LeastSquaresConverter",
      "org.apache.commons.math.optimization.general.GaussNewtonOptimizerTest$LinearProblem",
      "org.apache.commons.math.MaxIterationsExceededException",
      "org.apache.commons.math.optimization.general.MinpackTest$BrownAlmostLinearFunction",
      "org.apache.commons.math.linear.RealMatrixPreservingVisitor",
      "org.apache.commons.math.linear.Array2DRowRealMatrix",
      "org.apache.commons.math.optimization.general.MinpackTest$Box3DimensionalFunction",
      "org.apache.commons.math.analysis.DifferentiableMultivariateVectorialFunction",
      "org.apache.commons.math.linear.SparseRealMatrix",
      "org.apache.commons.math.linear.NonSquareMatrixException",
      "org.apache.commons.math.linear.BlockRealMatrixTest$GetVisitor",
      "org.apache.commons.math.linear.MatrixVisitorException",
      "org.apache.commons.math.FunctionEvaluationException",
      "org.apache.commons.math.linear.MatrixIndexException",
      "org.apache.commons.math.optimization.general.MinpackTest$BrownDennisFunction",
      "org.apache.commons.math.linear.AbstractRealMatrix",
      "org.apache.commons.math.linear.DefaultRealMatrixPreservingVisitor",
      "org.apache.commons.math.optimization.RealPointValuePair",
      "org.apache.commons.math.optimization.direct.MultiDirectional",
      "org.apache.commons.math.optimization.fitting.CurveFitter$TheoreticalValuesFunction",
      "org.apache.commons.math.linear.BlockRealMatrix",
      "org.apache.commons.math.linear.OpenMapRealVector",
      "org.apache.commons.math.linear.InvalidMatrixException",
      "org.apache.commons.math.linear.RealVector",
      "org.apache.commons.math.linear.RealMatrixChangingVisitor",
      "org.apache.commons.math.linear.Array2DRowRealMatrixTest$GetVisitor",
      "org.apache.commons.math.optimization.general.MinpackTest$MeyerFunction",
      "org.apache.commons.math.linear.OpenMapRealMatrix",
      "org.apache.commons.math.linear.BlockRealMatrixTest$SetVisitor",
      "org.apache.commons.math.optimization.direct.DirectSearchOptimizer",
      "org.apache.commons.math.optimization.general.MinpackTest$JennrichSampsonFunction",
      "org.apache.commons.math.optimization.GoalType"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("java.util.Comparator", false, MultiDirectional_ESTest_scaffolding.class.getClassLoader()));
  }
}
