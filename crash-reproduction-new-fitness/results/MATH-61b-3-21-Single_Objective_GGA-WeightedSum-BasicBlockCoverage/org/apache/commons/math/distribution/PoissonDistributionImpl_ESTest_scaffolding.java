/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon May 18 02:38:02 UTC 2020
 */

package org.apache.commons.math.distribution;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class PoissonDistributionImpl_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.apache.commons.math.distribution.PoissonDistributionImpl"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(PoissonDistributionImpl_ESTest_scaffolding.class.getClassLoader() ,
      "org.apache.commons.math.random.JDKRandomGenerator",
      "org.apache.commons.math.exception.NumberIsTooSmallException",
      "org.apache.commons.math.distribution.ChiSquaredDistribution",
      "org.apache.commons.math.MathException",
      "org.apache.commons.math.exception.NonMonotonousSequenceException",
      "org.apache.commons.math.distribution.ContinuousDistribution",
      "org.apache.commons.math.distribution.WeibullDistribution",
      "org.apache.commons.math.util.FastMath",
      "org.apache.commons.math.random.RandomAdaptorTest$ConstantGenerator",
      "org.apache.commons.math.util.MathUtils",
      "org.apache.commons.math.distribution.IntegerDistribution",
      "org.apache.commons.math.ConvergenceException",
      "org.apache.commons.math.exception.NotStrictlyPositiveException",
      "org.apache.commons.math.random.Well19937c",
      "org.apache.commons.math.distribution.PoissonDistribution",
      "org.apache.commons.math.random.Well19937a",
      "org.apache.commons.math.distribution.WeibullDistributionImpl",
      "org.apache.commons.math.analysis.UnivariateRealFunction",
      "org.apache.commons.math.distribution.PascalDistribution",
      "org.apache.commons.math.special.Gamma$1",
      "org.apache.commons.math.distribution.GammaDistribution",
      "org.apache.commons.math.util.ContinuedFraction",
      "org.apache.commons.math.distribution.Distribution",
      "org.apache.commons.math.random.RandomGenerator",
      "org.apache.commons.math.exception.MathIllegalArgumentException",
      "org.apache.commons.math.distribution.FDistributionImpl",
      "org.apache.commons.math.distribution.NormalDistribution",
      "org.apache.commons.math.distribution.SaddlePointExpansion",
      "org.apache.commons.math.distribution.HypergeometricDistributionImpl",
      "org.apache.commons.math.exception.MathIllegalNumberException",
      "org.apache.commons.math.MathRuntimeException",
      "org.apache.commons.math.distribution.BinomialDistribution",
      "org.apache.commons.math.distribution.ZipfDistributionImpl",
      "org.apache.commons.math.MathRuntimeException$1",
      "org.apache.commons.math.MathRuntimeException$2",
      "org.apache.commons.math.MathRuntimeException$3",
      "org.apache.commons.math.MathRuntimeException$4",
      "org.apache.commons.math.random.AbstractRandomGenerator",
      "org.apache.commons.math.random.Well44497b",
      "org.apache.commons.math.MathRuntimeException$5",
      "org.apache.commons.math.random.Well44497a",
      "org.apache.commons.math.MathRuntimeException$6",
      "org.apache.commons.math.distribution.NormalDistributionImpl",
      "org.apache.commons.math.MathRuntimeException$7",
      "org.apache.commons.math.MathRuntimeException$8",
      "org.apache.commons.math.MathRuntimeException$10",
      "org.apache.commons.math.MathRuntimeException$9",
      "org.apache.commons.math.MathRuntimeException$11",
      "org.apache.commons.math.distribution.ExponentialDistribution",
      "org.apache.commons.math.distribution.GammaDistributionImpl",
      "org.apache.commons.math.distribution.AbstractIntegerDistribution",
      "org.apache.commons.math.random.RandomData",
      "org.apache.commons.math.distribution.HasDensity",
      "org.apache.commons.math.random.MersenneTwister",
      "org.apache.commons.math.random.AbstractWell",
      "org.apache.commons.math.distribution.HypergeometricDistribution",
      "org.apache.commons.math.special.Erf",
      "org.apache.commons.math.random.RandomDataImpl",
      "org.apache.commons.math.distribution.BetaDistributionImpl",
      "org.apache.commons.math.distribution.PascalDistributionImpl",
      "org.apache.commons.math.exception.NumberIsTooLargeException",
      "org.apache.commons.math.distribution.BetaDistribution",
      "org.apache.commons.math.distribution.CauchyDistributionImpl",
      "org.apache.commons.math.MaxIterationsExceededException",
      "org.apache.commons.math.distribution.CauchyDistribution",
      "org.apache.commons.math.special.Gamma",
      "org.apache.commons.math.distribution.FDistribution",
      "org.apache.commons.math.exception.util.Localizable",
      "org.apache.commons.math.random.Well1024a",
      "org.apache.commons.math.random.Well512a",
      "org.apache.commons.math.FunctionEvaluationException",
      "org.apache.commons.math.distribution.TDistribution",
      "org.apache.commons.math.distribution.ExponentialDistributionImpl",
      "org.apache.commons.math.distribution.DiscreteDistribution",
      "org.apache.commons.math.distribution.BinomialDistributionImpl",
      "org.apache.commons.math.random.TestRandomGenerator",
      "org.apache.commons.math.random.BitsStreamGenerator",
      "org.apache.commons.math.distribution.TDistributionImpl",
      "org.apache.commons.math.distribution.AbstractContinuousDistribution",
      "org.apache.commons.math.exception.util.LocalizedFormats",
      "org.apache.commons.math.distribution.PoissonDistributionImpl",
      "org.apache.commons.math.distribution.ChiSquaredDistributionImpl",
      "org.apache.commons.math.random.RandomAdaptor",
      "org.apache.commons.math.distribution.ZipfDistribution",
      "org.apache.commons.math.distribution.AbstractDistribution"
    );
  } 
}
