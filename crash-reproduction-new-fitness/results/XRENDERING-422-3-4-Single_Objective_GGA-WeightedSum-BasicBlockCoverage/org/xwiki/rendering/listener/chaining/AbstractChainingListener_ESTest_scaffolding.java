/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Fri May 15 02:19:05 UTC 2020
 */

package org.xwiki.rendering.listener.chaining;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class AbstractChainingListener_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.rendering.listener.chaining.AbstractChainingListener"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(AbstractChainingListener_ESTest_scaffolding.class.getClassLoader() ,
      "org.xwiki.rendering.listener.chaining.EventType$28",
      "org.xwiki.rendering.listener.chaining.EventType$27",
      "org.xwiki.rendering.listener.chaining.EventType$26",
      "org.xwiki.rendering.listener.chaining.EventType$25",
      "org.xwiki.rendering.listener.chaining.EventType$29",
      "org.xwiki.rendering.listener.chaining.EventType$20",
      "org.xwiki.rendering.listener.chaining.BlockStateChainingListener$DefinitionListState",
      "org.xwiki.rendering.listener.chaining.EventType$24",
      "org.xwiki.rendering.listener.chaining.EventType$23",
      "org.xwiki.rendering.listener.chaining.EventType$22",
      "org.xwiki.rendering.listener.chaining.EventType$21",
      "org.xwiki.rendering.listener.chaining.AbstractChainingListener",
      "org.xwiki.rendering.listener.chaining.EventType$3",
      "org.xwiki.rendering.listener.chaining.EventType$4",
      "org.xwiki.rendering.listener.chaining.EventType$1",
      "org.xwiki.rendering.listener.chaining.EventType$2",
      "org.apache.commons.lang3.builder.ToStringStyle$NoClassNameToStringStyle",
      "org.xwiki.rendering.listener.chaining.BlockStateChainingListener$ListState",
      "org.xwiki.rendering.listener.chaining.EventType$39",
      "org.xwiki.rendering.listener.chaining.EventType$38",
      "org.xwiki.rendering.listener.chaining.EventType$37",
      "org.xwiki.rendering.listener.chaining.EventType$36",
      "org.xwiki.rendering.listener.chaining.BlockStateChainingListener",
      "org.xwiki.rendering.listener.chaining.EventType$31",
      "org.apache.commons.lang3.JavaVersion",
      "org.xwiki.rendering.listener.chaining.EventType$30",
      "org.xwiki.filter.annotation.Default",
      "org.xwiki.rendering.listener.chaining.EventType$35",
      "org.xwiki.rendering.listener.chaining.EventType$34",
      "org.xwiki.rendering.listener.chaining.EventType$33",
      "org.xwiki.rendering.listener.chaining.EventType$32",
      "org.xwiki.rendering.listener.reference.ResourceType",
      "org.xwiki.rendering.listener.Listener",
      "org.xwiki.rendering.listener.chaining.EventType$49",
      "org.xwiki.rendering.listener.chaining.EventType$48",
      "org.xwiki.rendering.listener.chaining.EventType$47",
      "org.xwiki.rendering.listener.QueueListener$Event",
      "org.xwiki.rendering.listener.chaining.EventType$42",
      "org.xwiki.rendering.listener.chaining.EventType$41",
      "org.xwiki.rendering.listener.chaining.EventType$40",
      "org.xwiki.rendering.listener.chaining.EventType$46",
      "org.xwiki.filter.annotation.Name",
      "org.xwiki.rendering.listener.chaining.EventType$45",
      "org.xwiki.rendering.listener.chaining.EventType$44",
      "org.apache.commons.lang3.builder.StandardToStringStyle",
      "org.xwiki.rendering.listener.chaining.EventType$43",
      "org.xwiki.rendering.listener.chaining.GroupStateChainingListener",
      "org.xwiki.rendering.listener.chaining.LookaheadChainingListenerTest",
      "org.apache.commons.lang3.SystemUtils",
      "org.xwiki.rendering.listener.Format",
      "org.xwiki.rendering.listener.chaining.EventType$9",
      "org.xwiki.rendering.listener.chaining.EventType$51",
      "org.xwiki.rendering.listener.chaining.EventType$50",
      "org.xwiki.rendering.listener.chaining.EventType$7",
      "org.xwiki.rendering.listener.chaining.EventType$8",
      "org.xwiki.rendering.listener.chaining.EventType$5",
      "org.xwiki.rendering.listener.chaining.EventType$6",
      "org.apache.commons.lang3.builder.ToStringStyle$SimpleToStringStyle",
      "org.xwiki.rendering.listener.ImageListener",
      "org.xwiki.rendering.listener.chaining.EventType",
      "org.xwiki.rendering.listener.chaining.ListenerChain",
      "org.xwiki.rendering.listener.chaining.LookaheadChainingListenerTest$TestChainingListener",
      "org.xwiki.rendering.listener.ListType",
      "org.apache.commons.lang3.builder.ToStringStyle$NoFieldNameToStringStyle",
      "org.xwiki.rendering.listener.chaining.ConsecutiveNewLineStateChainingListener",
      "org.xwiki.rendering.listener.QueueListener",
      "org.xwiki.rendering.syntax.Syntax",
      "org.apache.commons.lang3.builder.ToStringStyle$JsonToStringStyle",
      "org.xwiki.rendering.listener.HeaderLevel",
      "org.xwiki.rendering.listener.chaining.EmptyBlockChainingListener",
      "org.xwiki.rendering.listener.chaining.StackableChainingListener",
      "org.xwiki.rendering.listener.MetaData",
      "org.apache.commons.lang3.builder.ToStringBuilder",
      "org.apache.commons.lang3.builder.ToStringStyle$MultiLineToStringStyle",
      "org.xwiki.rendering.listener.reference.ResourceReference",
      "org.xwiki.rendering.listener.chaining.BlockStateChainingListener$Event",
      "org.apache.commons.lang3.builder.ToStringStyle",
      "org.apache.commons.lang3.builder.ToStringStyle$ShortPrefixToStringStyle",
      "org.xwiki.rendering.syntax.SyntaxType",
      "org.xwiki.rendering.listener.chaining.EventType$17",
      "org.xwiki.rendering.listener.chaining.EventType$16",
      "org.xwiki.rendering.listener.chaining.EventType$15",
      "org.xwiki.rendering.listener.chaining.EventType$14",
      "org.xwiki.rendering.listener.chaining.MetaDataStateChainingListener",
      "org.xwiki.rendering.listener.chaining.EventType$19",
      "org.xwiki.rendering.listener.chaining.EventType$18",
      "org.xwiki.rendering.listener.chaining.EventType$13",
      "org.xwiki.text.XWikiToStringStyle",
      "org.xwiki.rendering.listener.chaining.EventType$12",
      "org.xwiki.rendering.listener.chaining.EventType$11",
      "org.xwiki.rendering.listener.chaining.EventType$10",
      "org.apache.commons.lang3.builder.ToStringStyle$DefaultToStringStyle",
      "org.xwiki.rendering.listener.chaining.ChainingListener",
      "org.xwiki.rendering.listener.LinkListener",
      "org.apache.commons.lang3.builder.Builder",
      "org.xwiki.rendering.listener.chaining.LookaheadChainingListener",
      "org.xwiki.rendering.listener.chaining.BlockStateChainingListener$1"
    );
  } 
}
