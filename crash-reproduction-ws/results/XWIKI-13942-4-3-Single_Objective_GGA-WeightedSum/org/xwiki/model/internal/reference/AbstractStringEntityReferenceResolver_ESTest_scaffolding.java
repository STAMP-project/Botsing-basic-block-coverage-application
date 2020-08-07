/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon Mar 30 17:03:21 UTC 2020
 */

package org.xwiki.model.internal.reference;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class AbstractStringEntityReferenceResolver_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.model.internal.reference.AbstractStringEntityReferenceResolver"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(AbstractStringEntityReferenceResolver_ESTest_scaffolding.class.getClassLoader() ,
      "org.xwiki.model.internal.reference.LocalizedStringEntityReferenceSerializer",
      "org.xwiki.component.phase.Initializable",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceSerializer",
      "org.xwiki.component.annotation.Component",
      "org.xwiki.model.reference.EntityReference",
      "org.xwiki.model.internal.reference.DefaultSymbolScheme$1",
      "org.xwiki.model.internal.reference.SymbolScheme",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceResolver",
      "org.xwiki.text.StringUtils",
      "org.xwiki.component.phase.InitializationException",
      "org.xwiki.model.internal.reference.DefaultSymbolScheme",
      "org.apache.commons.lang3.StringUtils",
      "org.xwiki.model.reference.EntityReferenceResolver",
      "org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceSerializer",
      "org.xwiki.model.reference.DocumentReference",
      "org.xwiki.model.reference.LocalDocumentReference",
      "org.xwiki.model.internal.reference.AbstractEntityReferenceResolver",
      "org.xwiki.model.EntityType",
      "org.xwiki.model.reference.EntityReferenceSerializer"
    );
  } 
}
