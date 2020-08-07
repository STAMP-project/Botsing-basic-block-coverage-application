/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Tue Mar 31 14:32:51 UTC 2020
 */

package org.xwiki.extension.job.internal;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.extension.job.internal.AbstractInstallPlanJob$ModifableExtensionPlanTree"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest_scaffolding.class.getClassLoader() ,
      "org.xwiki.extension.job.internal.AbstractInstallPlanJob$ModifableExtensionPlanTree",
      "org.xwiki.job.Job",
      "org.xwiki.job.GroupedJob",
      "org.xwiki.extension.version.VersionConstraint",
      "org.xwiki.job.Request",
      "org.xwiki.extension.job.internal.AbstractInstallPlanJob$ModifableExtensionPlanNode",
      "org.xwiki.extension.job.plan.ExtensionPlanAction",
      "org.xwiki.extension.job.internal.AbstractExtensionJob",
      "org.xwiki.extension.job.internal.AbstractExtensionPlanJob",
      "org.xwiki.extension.job.plan.internal.DefaultExtensionPlanTree",
      "org.xwiki.extension.job.internal.AbstractInstallPlanJob",
      "org.xwiki.component.descriptor.ComponentInstantiationStrategy",
      "org.xwiki.component.annotation.InstantiationStrategy",
      "org.xwiki.job.AbstractJob",
      "org.xwiki.extension.job.ExtensionRequest",
      "org.xwiki.extension.job.plan.internal.DefaultExtensionPlanNode",
      "org.xwiki.extension.job.plan.ExtensionPlanNode",
      "org.xwiki.extension.job.plan.ExtensionPlanTree"
    );
  } 
}
