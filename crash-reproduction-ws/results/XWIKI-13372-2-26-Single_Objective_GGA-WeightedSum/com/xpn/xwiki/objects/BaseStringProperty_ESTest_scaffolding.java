/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Tue Mar 31 08:43:50 UTC 2020
 */

package com.xpn.xwiki.objects;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class BaseStringProperty_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "com.xpn.xwiki.objects.BaseStringProperty"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(BaseStringProperty_ESTest_scaffolding.class.getClassLoader() ,
      "com.xpn.xwiki.XWikiException",
      "org.dom4j.Document",
      "org.xwiki.bridge.DocumentModelBridge",
      "org.dom4j.Node",
      "org.xwiki.model.reference.EntityReference",
      "com.xpn.xwiki.objects.StringListProperty",
      "com.xpn.xwiki.objects.classes.ClassInterface",
      "com.xpn.xwiki.objects.ObjectInterface",
      "com.xpn.xwiki.doc.merge.MergeConfiguration",
      "com.xpn.xwiki.objects.LongProperty",
      "com.xpn.xwiki.objects.BaseCollection",
      "org.xwiki.model.reference.DocumentReferenceResolver",
      "com.xpn.xwiki.objects.StringProperty",
      "com.xpn.xwiki.objects.ObjectDiff",
      "org.xwiki.model.reference.EntityReferenceSerializer",
      "com.xpn.xwiki.objects.ListProperty",
      "com.xpn.xwiki.objects.DBStringListProperty",
      "com.xpn.xwiki.objects.NumberProperty",
      "com.xpn.xwiki.objects.BaseObject",
      "com.xpn.xwiki.objects.LargeStringProperty",
      "com.xpn.xwiki.objects.FloatProperty",
      "com.xpn.xwiki.objects.BaseProperty",
      "org.dom4j.Branch",
      "com.xpn.xwiki.doc.merge.MergeResult",
      "com.xpn.xwiki.objects.BaseElement",
      "com.xpn.xwiki.objects.BaseStringProperty",
      "org.xwiki.model.reference.ObjectPropertyReference",
      "com.xpn.xwiki.objects.DoubleProperty",
      "org.xwiki.model.reference.EntityReferenceResolver",
      "org.xwiki.localization.ContextualLocalizationManager",
      "org.xwiki.model.reference.DocumentReference",
      "com.xpn.xwiki.XWikiContext",
      "com.xpn.xwiki.objects.IntegerProperty",
      "org.xwiki.model.reference.AttachmentReference",
      "com.xpn.xwiki.objects.PropertyInterface",
      "org.dom4j.Element",
      "com.xpn.xwiki.doc.XWikiDocument",
      "com.xpn.xwiki.objects.classes.BaseClass",
      "com.xpn.xwiki.objects.ElementInterface"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("com.xpn.xwiki.objects.BaseCollection", false, BaseStringProperty_ESTest_scaffolding.class.getClassLoader()));
  }
}
