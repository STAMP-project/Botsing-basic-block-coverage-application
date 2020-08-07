/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Tue Mar 31 02:36:07 UTC 2020
 */

package com.xpn.xwiki.internal.skin;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class AbstractSkin_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "com.xpn.xwiki.internal.skin.AbstractSkin"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(AbstractSkin_ESTest_scaffolding.class.getClassLoader() ,
      "org.xwiki.rendering.parser.ParseException",
      "org.apache.commons.configuration.Configuration",
      "com.xpn.xwiki.internal.skin.InternalSkinConfiguration",
      "org.xwiki.component.phase.Initializable",
      "org.xwiki.component.annotation.Component",
      "org.xwiki.model.reference.EntityReference",
      "org.xwiki.component.phase.InitializationException",
      "org.xwiki.skin.Resource",
      "com.xpn.xwiki.internal.skin.AbstractResource",
      "com.xpn.xwiki.internal.skin.WikiSkin",
      "org.hibernate.loader.custom.sql.SQLCustomQuery",
      "com.xpn.xwiki.internal.skin.AbstractEnvironmentResource",
      "org.xwiki.rendering.syntax.Syntax",
      "org.xwiki.component.annotation.Role",
      "org.apache.commons.lang.exception.NestableException",
      "org.hibernate.loader.custom.CustomQuery",
      "com.xpn.xwiki.internal.skin.InternalSkinManager",
      "org.xwiki.skin.Skin",
      "com.xpn.xwiki.internal.ReadOnlyXWikiContextProvider",
      "org.xwiki.context.Execution",
      "org.xwiki.environment.Environment",
      "com.xpn.xwiki.internal.skin.AbstractSkin$1",
      "org.xwiki.cache.config.CacheConfiguration",
      "org.hibernate.loader.custom.sql.SQLQueryParser$ParserContext",
      "com.xpn.xwiki.internal.skin.AbstractSkin",
      "org.xwiki.cache.config.LRUCacheConfiguration",
      "org.xwiki.skin.ResourceRepository",
      "com.xpn.xwiki.internal.skin.SkinEnvironmentResource",
      "org.apache.commons.lang.exception.Nestable",
      "org.xwiki.rendering.syntax.SyntaxFactory",
      "org.xwiki.context.ExecutionContext",
      "org.xwiki.model.reference.DocumentReference",
      "com.xpn.xwiki.XWikiContext",
      "org.xwiki.observation.EventListener",
      "org.apache.commons.configuration.ConfigurationException",
      "org.xwiki.cache.CacheException",
      "com.xpn.xwiki.internal.skin.EnvironmentSkin",
      "org.xwiki.cache.Cache"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("com.xpn.xwiki.internal.skin.InternalSkinConfiguration", false, AbstractSkin_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("com.xpn.xwiki.internal.skin.InternalSkinManager", false, AbstractSkin_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.context.Execution", false, AbstractSkin_ESTest_scaffolding.class.getClassLoader()));
  }
}
