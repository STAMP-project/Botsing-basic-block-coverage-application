/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon May 18 01:31:27 UTC 2020
 */

package org.apache.commons.lang.text;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class StrBuilder_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.apache.commons.lang.text.StrBuilder"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(StrBuilder_ESTest_scaffolding.class.getClassLoader() ,
      "org.apache.commons.lang.text.StrBuilder",
      "org.apache.commons.lang.text.StrBuilder$StrBuilderTokenizer",
      "org.apache.commons.lang.text.StrTokenizer",
      "org.apache.commons.lang.text.StrBuilder$StrBuilderWriter",
      "org.apache.commons.lang.text.StrMatcher$CharMatcher",
      "org.apache.commons.lang.text.StrMatcher$TrimMatcher",
      "org.apache.commons.lang.text.StrMatcher$CharSetMatcher",
      "org.apache.commons.lang.text.StrBuilder$StrBuilderReader",
      "org.apache.commons.lang.text.StrMatcher$StringMatcher",
      "org.apache.commons.lang.text.StrMatcher$NoMatcher",
      "org.apache.commons.lang.text.StrMatcher",
      "org.apache.commons.lang.SystemUtils",
      "org.apache.commons.lang.ArrayUtils"
    );
  } 
}
