/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon May 18 00:38:42 UTC 2020
 */

package org.xwiki.model.reference;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class AttachmentReference_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.model.reference.AttachmentReference"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(AttachmentReference_ESTest_scaffolding.class.getClassLoader() ,
      "org.jfree.ui.FilesystemFilter",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceSerializer",
      "org.xwiki.stability.Unstable",
      "org.xwiki.component.annotation.Component",
      "org.xwiki.model.reference.EntityReference",
      "org.xwiki.text.StringUtils",
      "org.xwiki.model.reference.SpaceReference",
      "org.apache.commons.lang3.StringUtils",
      "org.xwiki.model.reference.LocalDocumentReference",
      "info.informatica.io.FilePatternSpec$FilePattern",
      "org.xwiki.model.EntityType",
      "org.xwiki.model.reference.EntityReferenceSerializer",
      "org.xwiki.model.internal.reference.LocalizedStringEntityReferenceSerializer",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$3",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$4",
      "org.xwiki.model.internal.reference.StringReferenceSeparators",
      "info.informatica.io.FilePatternSpec",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$1",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$2",
      "info.informatica.io.FilesystemInfo$TokenizedPath",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceSerializer",
      "org.xwiki.component.util.DefaultParameterizedType",
      "info.informatica.io.WildcardFilter",
      "org.xwiki.model.reference.DocumentReference",
      "org.xwiki.model.reference.AttachmentReference",
      "org.xwiki.model.reference.WikiReference",
      "info.informatica.io.FilesystemInfo"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.xwiki.model.reference.DocumentReference", false, AttachmentReference_ESTest_scaffolding.class.getClassLoader()));
  }
}
