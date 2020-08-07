/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sat Jan 18 06:32:36 UTC 2020
 */

package org.joda.time.field;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class FieldUtils_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.joda.time.field.FieldUtils"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(FieldUtils_ESTest_scaffolding.class.getClassLoader() ,
      "org.joda.time.field.TestBaseDateTimeField$MockCountingDurationField",
      "org.joda.time.field.BaseDateTimeField",
      "org.joda.time.base.AbstractPartial",
      "org.joda.time.base.BasePartial",
      "org.joda.time.DurationField",
      "org.joda.time.Chronology",
      "org.joda.time.field.TestBaseDateTimeField$MockStandardBaseDateTimeField",
      "org.joda.time.IllegalFieldValueException",
      "org.joda.time.field.TestBaseDateTimeField",
      "org.joda.time.field.TestBaseDateTimeField$MockBaseDateTimeField",
      "org.joda.time.field.TestBaseDateTimeField$MockPartial",
      "org.joda.time.DurationFieldType",
      "org.joda.time.ReadablePartial",
      "org.joda.time.DateTimeFieldType",
      "org.joda.time.DateTimeField",
      "org.joda.time.field.FieldUtils",
      "org.joda.time.DurationFieldType$StandardDurationFieldType",
      "org.joda.time.field.BaseDurationField",
      "org.joda.time.DateTimeFieldType$StandardDateTimeFieldType"
    );
  } 
}
