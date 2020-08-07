/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Tue Mar 31 14:45:58 UTC 2020
 */

package org.xwiki.extension.job.internal;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest_scaffolding.class.getClassLoader() ,
      "org.xwiki.extension.job.internal.AbstractInstallPlanJob$ModifableExtensionPlanTree",
      "org.xwiki.job.GroupedJob",
      "org.xwiki.job.Request",
      "org.xwiki.extension.version.Version$Type",
      "org.xwiki.extension.version.Version",
      "org.xwiki.extension.version.internal.DefaultVersionConstraint",
      "org.xwiki.extension.ExtensionException",
      "org.xwiki.extension.version.VersionRange",
      "org.xwiki.extension.job.plan.ExtensionPlanAction",
      "org.xwiki.extension.job.internal.AbstractExtensionPlanJob",
      "org.xwiki.extension.job.plan.internal.DefaultExtensionPlanTree",
      "org.xwiki.extension.job.internal.AbstractInstallPlanJob",
      "org.xwiki.component.annotation.InstantiationStrategy",
      "org.xwiki.job.AbstractJob",
      "org.xwiki.extension.job.ExtensionRequest",
      "org.xwiki.extension.job.plan.ExtensionPlanTree",
      "org.xwiki.extension.version.InvalidVersionConstraintException",
      "org.xwiki.job.Job",
      "org.xwiki.extension.version.VersionConstraint",
      "org.xwiki.extension.Extension",
      "org.xwiki.extension.InstalledExtension",
      "org.xwiki.extension.job.internal.AbstractInstallPlanJob$ModifableExtensionPlanNode",
      "org.xwiki.extension.LocalExtension",
      "org.xwiki.extension.job.internal.AbstractExtensionJob",
      "org.xwiki.extension.version.internal.DefaultVersion",
      "org.xwiki.extension.version.VersionRangeCollection",
      "org.xwiki.component.descriptor.ComponentInstantiationStrategy",
      "org.xwiki.extension.job.plan.internal.DefaultExtensionPlanNode",
      "org.xwiki.extension.version.InvalidVersionRangeException",
      "org.xwiki.extension.job.plan.ExtensionPlanAction$Action",
      "org.xwiki.extension.version.IncompatibleVersionConstraintException",
      "org.xwiki.extension.job.plan.ExtensionPlanNode"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.xwiki.extension.job.plan.ExtensionPlanAction", false, AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.extension.version.Version", false, AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest_scaffolding.class.getClassLoader()));
  }
}
