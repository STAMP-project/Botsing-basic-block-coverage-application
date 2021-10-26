/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Tue Oct 26 00:10:40 UTC 2021
 */

package org.xwiki.rendering.listener.chaining;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class EmptyBlockChainingListener_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.rendering.listener.chaining.EmptyBlockChainingListener"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(EmptyBlockChainingListener_ESTest_scaffolding.class.getClassLoader() ,
      "org.xwiki.rendering.listener.reference.ResourceReference",
      "org.xwiki.rendering.listener.chaining.ConsecutiveNewLineStateChainingListener",
      "org.xwiki.rendering.listener.chaining.BlockStateChainingListener$Event",
      "org.xwiki.rendering.listener.QueueListener$Event",
      "org.apache.commons.lang3.builder.ToStringStyle",
      "org.xwiki.rendering.listener.QueueListener",
      "org.xwiki.rendering.listener.chaining.BlockStateChainingListener$DefinitionListState",
      "org.apache.commons.lang3.builder.StandardToStringStyle",
      "org.xwiki.rendering.listener.chaining.AbstractChainingListener",
      "org.xwiki.rendering.syntax.Syntax",
      "org.xwiki.rendering.listener.chaining.BlockStateChainingListener$ListState",
      "org.xwiki.rendering.listener.chaining.GroupStateChainingListener",
      "org.xwiki.rendering.listener.HeaderLevel",
      "org.xwiki.rendering.listener.chaining.LookaheadChainingListenerTest",
      "org.xwiki.rendering.syntax.SyntaxType",
      "org.xwiki.rendering.listener.chaining.EmptyBlockChainingListener",
      "org.xwiki.rendering.listener.chaining.BlockStateChainingListener",
      "org.xwiki.rendering.listener.chaining.MetaDataStateChainingListener",
      "org.xwiki.rendering.listener.Format",
      "org.xwiki.rendering.listener.chaining.StackableChainingListener",
      "org.xwiki.text.XWikiToStringStyle",
      "org.xwiki.rendering.listener.ImageListener",
      "org.xwiki.rendering.listener.chaining.ListenerChain",
      "org.xwiki.rendering.listener.MetaData",
      "org.xwiki.rendering.listener.reference.ResourceType",
      "org.xwiki.rendering.listener.chaining.ChainingListener",
      "org.xwiki.rendering.listener.LinkListener",
      "org.xwiki.rendering.listener.chaining.LookaheadChainingListenerTest$TestChainingListener",
      "org.xwiki.rendering.listener.ListType",
      "org.xwiki.rendering.listener.Listener",
      "org.xwiki.rendering.listener.chaining.LookaheadChainingListener",
      "org.xwiki.rendering.listener.chaining.BlockStateChainingListener$1"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.xwiki.rendering.listener.chaining.ListenerChain", false, EmptyBlockChainingListener_ESTest_scaffolding.class.getClassLoader()));
  }
}
