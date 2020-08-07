/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon May 18 01:36:22 UTC 2020
 */

package org.apache.commons.math3.fraction;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class BigFraction_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.apache.commons.math3.fraction.BigFraction"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(BigFraction_ESTest_scaffolding.class.getClassLoader() ,
      "org.apache.commons.math3.fraction.BigFractionField",
      "org.apache.commons.math3.exception.util.ExceptionContextProvider",
      "org.apache.commons.math3.fraction.BigFraction",
      "org.apache.commons.math3.exception.util.ArgUtils",
      "org.apache.commons.math3.exception.MathArithmeticException",
      "org.apache.commons.math3.exception.NumberIsTooSmallException",
      "org.apache.commons.math3.util.FastMath$ExpIntTable",
      "org.apache.commons.math3.util.FastMath$lnMant",
      "org.apache.commons.math3.exception.NotPositiveException",
      "org.apache.commons.math3.exception.MathIllegalStateException",
      "org.apache.commons.math3.util.FastMath$ExpFracTable",
      "org.apache.commons.math3.exception.MathIllegalArgumentException",
      "org.apache.commons.math3.util.MathUtils",
      "org.apache.commons.math3.exception.MathIllegalNumberException",
      "org.apache.commons.math3.exception.util.LocalizedFormats",
      "org.apache.commons.math3.exception.ZeroException",
      "org.apache.commons.math3.exception.ConvergenceException",
      "org.apache.commons.math3.util.FastMath",
      "org.apache.commons.math3.FieldElement",
      "org.apache.commons.math3.exception.util.Localizable",
      "org.apache.commons.math3.fraction.FractionConversionException",
      "org.apache.commons.math3.exception.util.ExceptionContext",
      "org.apache.commons.math3.util.ArithmeticUtils",
      "org.apache.commons.math3.exception.NullArgumentException",
      "org.apache.commons.math3.Field",
      "org.apache.commons.math3.exception.NotFiniteNumberException",
      "org.apache.commons.math3.util.FastMathLiteralArrays",
      "org.apache.commons.math3.fraction.BigFractionField$LazyHolder"
    );
  } 
}
