/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sat May 16 21:22:41 UTC 2020
 */

package org.apache.commons.lang3.text.translate;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class NumericEntityUnescaper_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.apache.commons.lang3.text.translate.NumericEntityUnescaper"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(NumericEntityUnescaper_ESTest_scaffolding.class.getClassLoader() ,
      "org.apache.commons.lang3.text.translate.NumericEntityUnescaper",
      "org.apache.commons.lang3.text.translate.NumericEntityEscaper",
      "org.apache.commons.lang3.text.translate.CharSequenceTranslator",
      "org.apache.commons.lang3.text.translate.AggregateTranslator",
      "org.apache.commons.lang3.text.translate.LookupTranslator",
      "org.apache.commons.lang3.text.translate.CodePointTranslator",
      "org.apache.commons.lang3.ArrayUtils",
      "org.apache.commons.lang3.text.translate.UnicodeEscaper",
      "org.apache.commons.lang3.text.translate.UnicodeUnescaper$OPTION",
      "org.apache.commons.lang3.text.translate.OctalUnescaper",
      "org.apache.commons.lang3.text.translate.UnicodeUnescaper"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("java.lang.CharSequence", false, NumericEntityUnescaper_ESTest_scaffolding.class.getClassLoader()));
  }
}
