/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon Oct 25 20:59:30 UTC 2021
 */

package org.apache.commons.math3.util;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class MathArrays_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.apache.commons.math3.util.MathArrays"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(MathArrays_ESTest_scaffolding.class.getClassLoader() ,
      "org.apache.commons.math3.fraction.BigFractionField",
      "org.apache.commons.math3.dfp.DfpField",
      "org.apache.commons.math3.util.Precision",
      "org.apache.commons.math3.exception.util.ExceptionContextProvider",
      "org.apache.commons.math3.dfp.DfpField$RoundingMode",
      "org.apache.commons.math3.fraction.BigFraction",
      "org.apache.commons.math3.util.MathArrays",
      "org.apache.commons.math3.util.MathArrays$1",
      "org.apache.commons.math3.util.MathArrays$2",
      "org.apache.commons.math3.util.MathArrays$3",
      "org.apache.commons.math3.fraction.FractionField",
      "org.apache.commons.math3.exception.util.ArgUtils",
      "org.apache.commons.math3.exception.MathArithmeticException",
      "org.apache.commons.math3.util.MathArrays$OrderDirection",
      "org.apache.commons.math3.complex.Complex",
      "org.apache.commons.math3.exception.NumberIsTooSmallException",
      "org.apache.commons.math3.fraction.FractionField$1",
      "org.apache.commons.math3.util.BigRealField",
      "org.apache.commons.math3.exception.NotPositiveException",
      "org.apache.commons.math3.exception.MathInternalError",
      "org.apache.commons.math3.exception.MathIllegalStateException",
      "org.apache.commons.math3.dfp.Dfp",
      "org.apache.commons.math3.exception.NonMonotonicSequenceException",
      "org.apache.commons.math3.RealFieldElement",
      "org.apache.commons.math3.exception.MathIllegalArgumentException",
      "org.apache.commons.math3.complex.ComplexField",
      "org.apache.commons.math3.util.MathUtils",
      "org.apache.commons.math3.exception.MathIllegalNumberException",
      "org.apache.commons.math3.util.Pair",
      "org.apache.commons.math3.exception.util.LocalizedFormats",
      "org.apache.commons.math3.util.Decimal64Field",
      "org.apache.commons.math3.util.BigRealField$1",
      "org.apache.commons.math3.util.FastMath",
      "org.apache.commons.math3.exception.DimensionMismatchException",
      "org.apache.commons.math3.FieldElement",
      "org.apache.commons.math3.exception.util.Localizable",
      "org.apache.commons.math3.fraction.BigFractionField$1",
      "org.apache.commons.math3.util.BigReal",
      "org.apache.commons.math3.util.Decimal64",
      "org.apache.commons.math3.exception.NotStrictlyPositiveException",
      "org.apache.commons.math3.fraction.Fraction",
      "org.apache.commons.math3.complex.ComplexField$1",
      "org.apache.commons.math3.exception.util.ExceptionContext",
      "org.apache.commons.math3.exception.NullArgumentException",
      "org.apache.commons.math3.exception.NoDataException",
      "org.apache.commons.math3.Field",
      "org.apache.commons.math3.exception.NotFiniteNumberException",
      "org.apache.commons.math3.util.MathArrays$Function"
    );
  } 
}
