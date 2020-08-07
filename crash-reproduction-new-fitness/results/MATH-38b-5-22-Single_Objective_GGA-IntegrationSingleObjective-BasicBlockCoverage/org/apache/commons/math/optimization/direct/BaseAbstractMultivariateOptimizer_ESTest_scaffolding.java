/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon May 18 02:31:40 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class BaseAbstractMultivariateOptimizer_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.apache.commons.math.optimization.direct.BaseAbstractMultivariateOptimizer"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(BaseAbstractMultivariateOptimizer_ESTest_scaffolding.class.getClassLoader() ,
      "org.apache.commons.math.optimization.direct.CMAESOptimizer$FitnessFunction",
      "org.apache.commons.math.exception.NumberIsTooSmallException",
      "org.apache.commons.math.exception.NullArgumentException",
      "org.apache.commons.math.exception.util.ExceptionContext",
      "org.apache.commons.math.optimization.univariate.BracketFinder",
      "org.apache.commons.math.util.FastMath",
      "org.apache.commons.math.util.MathUtils",
      "org.apache.commons.math.linear.TriDiagonalTransformer",
      "org.apache.commons.math.optimization.SimpleScalarValueChecker",
      "org.apache.commons.math.optimization.AbstractConvergenceChecker",
      "org.apache.commons.math.linear.MatrixUtils",
      "org.apache.commons.math.random.Well19937c",
      "org.apache.commons.math.random.Well19937a",
      "org.apache.commons.math.optimization.SimpleVectorialPointChecker",
      "org.apache.commons.math.optimization.direct.BOBYQAOptimizer$PathIsExploredException",
      "org.apache.commons.math.random.ISAACRandom",
      "org.apache.commons.math.optimization.direct.NelderMeadSimplex",
      "org.apache.commons.math.exception.NotPositiveException",
      "org.apache.commons.math.util.Precision",
      "org.apache.commons.math.linear.MatrixDimensionMismatchException",
      "org.apache.commons.math.linear.RealLinearOperator",
      "org.apache.commons.math.exception.MathIllegalArgumentException",
      "org.apache.commons.math.random.RandomAdaptorTest",
      "org.apache.commons.math.util.CompositeFormat",
      "org.apache.commons.math.optimization.direct.SimplexOptimizer",
      "org.apache.commons.math.exception.DimensionMismatchException",
      "org.apache.commons.math.optimization.univariate.BaseUnivariateOptimizer",
      "org.apache.commons.math.optimization.univariate.BaseAbstractUnivariateOptimizer",
      "org.apache.commons.math.linear.ArrayRealVector",
      "org.apache.commons.math.optimization.direct.PowellOptimizer",
      "org.apache.commons.math.optimization.direct.SimplexOptimizer$1",
      "org.apache.commons.math.optimization.direct.SimplexOptimizer$2",
      "org.apache.commons.math.random.AbstractRandomGenerator",
      "org.apache.commons.math.random.Well44497b",
      "org.apache.commons.math.analysis.MultivariateFunction",
      "org.apache.commons.math.random.Well44497a",
      "org.apache.commons.math.linear.NonSymmetricMatrixException",
      "org.apache.commons.math.util.FastMath$lnMant",
      "org.apache.commons.math.optimization.direct.CMAESOptimizer",
      "org.apache.commons.math.random.MersenneTwister",
      "org.apache.commons.math.random.AbstractWell",
      "org.apache.commons.math.linear.DecompositionSolver",
      "org.apache.commons.math.random.RandomDataTest",
      "org.apache.commons.math.util.FastMathLiteralArrays",
      "org.apache.commons.math.linear.RealVectorFormat",
      "org.apache.commons.math.exception.OutOfRangeException",
      "org.apache.commons.math.linear.RealMatrixPreservingVisitor",
      "org.apache.commons.math.analysis.SincFunction",
      "org.apache.commons.math.optimization.BaseOptimizer",
      "org.apache.commons.math.linear.SparseRealMatrix",
      "org.apache.commons.math.analysis.SumSincFunction",
      "org.apache.commons.math.linear.FieldMatrixPreservingVisitor",
      "org.apache.commons.math.exception.util.Localizable",
      "org.apache.commons.math.analysis.DifferentiableUnivariateFunction",
      "org.apache.commons.math.random.Well512a",
      "org.apache.commons.math.util.MathArrays",
      "org.apache.commons.math.optimization.direct.AbstractSimplex",
      "org.apache.commons.math.linear.AbstractRealMatrix",
      "org.apache.commons.math.exception.MaxCountExceededException",
      "org.apache.commons.math.optimization.VectorialPointValuePair",
      "org.apache.commons.math.random.TestRandomGenerator",
      "org.apache.commons.math.analysis.MultivariateVectorFunction",
      "org.apache.commons.math.random.BitsStreamGenerator",
      "org.apache.commons.math.linear.BlockRealMatrix",
      "org.apache.commons.math.optimization.BaseMultivariateSimpleBoundsOptimizer",
      "org.apache.commons.math.linear.RealVector",
      "org.apache.commons.math.optimization.direct.BaseAbstractMultivariateSimpleBoundsOptimizer",
      "org.apache.commons.math.linear.RealMatrixChangingVisitor",
      "org.apache.commons.math.optimization.univariate.UnivariateRealOptimizer",
      "org.apache.commons.math.random.RandomGeneratorAbstractTest",
      "org.apache.commons.math.linear.OpenMapRealMatrix",
      "org.apache.commons.math.optimization.GoalType",
      "org.apache.commons.math.exception.util.ArgUtils",
      "org.apache.commons.math.exception.MathIllegalStateException",
      "org.apache.commons.math.random.JDKRandomGenerator",
      "org.apache.commons.math.linear.BlockFieldMatrix",
      "org.apache.commons.math.util.Incrementor",
      "org.apache.commons.math.util.Incrementor$MaxCountExceededCallback",
      "org.apache.commons.math.random.RandomAdaptorTest$ConstantGenerator",
      "org.apache.commons.math.linear.AbstractRealMatrix$5",
      "org.apache.commons.math.linear.AbstractRealMatrix$2",
      "org.apache.commons.math.exception.NonMonotonicSequenceException",
      "org.apache.commons.math.exception.NotStrictlyPositiveException",
      "org.apache.commons.math.exception.NotFiniteNumberException",
      "org.apache.commons.math.linear.RealMatrix",
      "org.apache.commons.math.random.RandomGenerator",
      "org.apache.commons.math.optimization.univariate.BrentOptimizer",
      "org.apache.commons.math.random.BitsStreamGeneratorTest$TestBitStreamGenerator",
      "org.apache.commons.math.linear.AbstractFieldMatrix",
      "org.apache.commons.math.exception.MultiDimensionMismatchException",
      "org.apache.commons.math.exception.MathParseException",
      "org.apache.commons.math.optimization.direct.BaseAbstractMultivariateOptimizer",
      "org.apache.commons.math.exception.MathIllegalNumberException",
      "org.apache.commons.math.optimization.direct.CMAESOptimizer$DoubleIndex",
      "org.apache.commons.math.optimization.MultivariateOptimizer",
      "org.apache.commons.math.optimization.SimpleRealPointChecker",
      "org.apache.commons.math.analysis.BivariateRealFunction",
      "org.apache.commons.math.optimization.SimpleVectorialValueChecker",
      "org.apache.commons.math.analysis.SumSincFunction$1",
      "org.apache.commons.math.analysis.SumSincFunction$2",
      "org.apache.commons.math.optimization.ConvergenceChecker",
      "org.apache.commons.math.analysis.UnivariateFunction",
      "org.apache.commons.math.linear.AnyMatrix",
      "org.apache.commons.math.exception.NumberIsTooLargeException",
      "org.apache.commons.math.exception.NoDataException",
      "org.apache.commons.math.linear.RealVector$2",
      "org.apache.commons.math.optimization.univariate.UnivariateRealPointValuePair",
      "org.apache.commons.math.exception.MathInternalError",
      "org.apache.commons.math.exception.ZeroException",
      "org.apache.commons.math.exception.TooManyEvaluationsException",
      "org.apache.commons.math.linear.Array2DRowRealMatrix",
      "org.apache.commons.math.linear.EigenDecomposition",
      "org.apache.commons.math.optimization.BaseMultivariateOptimizer",
      "org.apache.commons.math.optimization.direct.PowellOptimizer$LineSearch",
      "org.apache.commons.math.random.Well1024a",
      "org.apache.commons.math.optimization.direct.MultiDirectionalSimplex",
      "org.apache.commons.math.linear.NonSquareMatrixException",
      "org.apache.commons.math.random.BitsStreamGeneratorTest$BitRandom",
      "org.apache.commons.math.linear.DefaultRealMatrixPreservingVisitor",
      "org.apache.commons.math.util.Incrementor$1",
      "org.apache.commons.math.optimization.RealPointValuePair",
      "org.apache.commons.math.random.BitsStreamGeneratorTest",
      "org.apache.commons.math.linear.FieldVector",
      "org.apache.commons.math.exception.MathArithmeticException",
      "org.apache.commons.math.analysis.DifferentiableMultivariateFunction",
      "org.apache.commons.math.linear.Array2DRowFieldMatrix",
      "org.apache.commons.math.exception.util.LocalizedFormats",
      "org.apache.commons.math.analysis.SincFunction$1",
      "org.apache.commons.math.linear.FieldMatrix",
      "org.apache.commons.math.optimization.direct.PowellOptimizer$LineSearch$1",
      "org.apache.commons.math.optimization.direct.BOBYQAOptimizer",
      "org.apache.commons.math.exception.util.ExceptionContextProvider",
      "org.apache.commons.math.RetryRunner",
      "org.apache.commons.math.random.RandomAdaptor"
    );
  } 
}
