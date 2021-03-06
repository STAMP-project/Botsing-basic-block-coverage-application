/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Tue Mar 31 00:04:35 UTC 2020
 */

package org.xwiki.rendering.internal.parser;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class DefaultContentParser_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.rendering.internal.parser.DefaultContentParser"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(DefaultContentParser_ESTest_scaffolding.class.getClassLoader() ,
      "org.xwiki.rendering.parser.ParseException",
      "org.xwiki.rendering.block.AbstractBlock",
      "org.xwiki.rendering.block.XDOM",
      "org.xwiki.component.annotation.Component",
      "org.xwiki.model.reference.EntityReference",
      "org.xwiki.rendering.internal.parser.DefaultContentParser",
      "org.xwiki.component.manager.ComponentLookupException",
      "org.xwiki.rendering.internal.parser.creole.CreoleParser",
      "org.xwiki.rendering.syntax.Syntax",
      "org.xwiki.rendering.block.Block",
      "org.xwiki.component.annotation.Role",
      "org.xwiki.rendering.parser.MissingParserException",
      "org.xwiki.component.manager.ComponentManager",
      "org.xwiki.rendering.internal.parser.wikimodel.WikiModelStreamParser",
      "org.xwiki.component.manager.CompatibilityComponentManager",
      "org.xwiki.rendering.internal.parser.confluence.ConfluenceParser",
      "org.xwiki.rendering.internal.parser.wikimodel.AbstractWikiModelParser",
      "org.xwiki.rendering.parser.ContentParser",
      "org.xwiki.model.reference.EntityReferenceSerializer",
      "org.xwiki.rendering.syntax.SyntaxType",
      "org.xwiki.rendering.parser.Parser",
      "org.xwiki.rendering.block.MetaDataBlock",
      "org.xwiki.rendering.parser.StreamParser"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("javax.inject.Provider", false, DefaultContentParser_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.model.reference.EntityReferenceSerializer", false, DefaultContentParser_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.rendering.syntax.Syntax", false, DefaultContentParser_ESTest_scaffolding.class.getClassLoader()));
  }
}
