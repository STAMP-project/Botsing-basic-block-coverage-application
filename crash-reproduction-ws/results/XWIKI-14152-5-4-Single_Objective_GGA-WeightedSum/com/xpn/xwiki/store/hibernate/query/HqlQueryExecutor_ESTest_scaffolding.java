/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon Mar 30 16:47:48 UTC 2020
 */

package com.xpn.xwiki.store.hibernate.query;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class HqlQueryExecutor_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(HqlQueryExecutor_ESTest_scaffolding.class.getClassLoader() ,
      "com.xpn.xwiki.XWikiException",
      "org.xwiki.query.QueryExecutor",
      "org.xwiki.component.phase.Initializable",
      "org.xwiki.query.QueryException",
      "org.xwiki.query.QueryFilter",
      "org.xwiki.query.Query",
      "org.xwiki.query.internal.CountDocumentFilter",
      "com.xpn.xwiki.store.XWikiHibernateStore",
      "org.xwiki.component.annotation.Component",
      "org.hibernate.Session",
      "com.xpn.xwiki.store.XWikiStoreInterface",
      "org.xwiki.component.phase.InitializationException",
      "com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor",
      "org.xwiki.query.internal.AbstractQueryFilter",
      "org.xwiki.query.internal.DefaultQuery",
      "com.xpn.xwiki.XWikiContext",
      "com.xpn.xwiki.store.XWikiHibernateBaseStore",
      "org.hibernate.Query",
      "org.xwiki.query.SecureQuery",
      "com.xpn.xwiki.store.XWikiHibernateBaseStore$HibernateCallback"
    );
  } 
}
