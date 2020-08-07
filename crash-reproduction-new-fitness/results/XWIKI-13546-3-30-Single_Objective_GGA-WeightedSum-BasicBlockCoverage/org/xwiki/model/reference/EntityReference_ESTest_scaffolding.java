/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sun May 17 18:09:54 UTC 2020
 */

package org.xwiki.model.reference;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class EntityReference_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.model.reference.EntityReference"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(EntityReference_ESTest_scaffolding.class.getClassLoader() ,
      "org.xwiki.model.internal.reference.LocalizedStringEntityReferenceSerializer",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$3",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceSerializer",
      "org.xwiki.stability.Unstable",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$4",
      "org.xwiki.component.annotation.Component",
      "org.xwiki.model.reference.EntityReference",
      "org.xwiki.model.internal.reference.StringReferenceSeparators",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$1",
      "org.xwiki.text.StringUtils",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$2",
      "org.apache.commons.lang3.StringUtils",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceSerializer",
      "org.apache.commons.lang3.Validate",
      "org.xwiki.model.reference.DocumentReference",
      "org.xwiki.model.reference.LocalDocumentReference",
      "org.apache.commons.lang3.builder.Builder",
      "org.apache.commons.lang3.builder.HashCodeBuilder",
      "org.xwiki.model.EntityType",
      "org.xwiki.model.reference.EntityReferenceSerializer"
    );
  } 
}
