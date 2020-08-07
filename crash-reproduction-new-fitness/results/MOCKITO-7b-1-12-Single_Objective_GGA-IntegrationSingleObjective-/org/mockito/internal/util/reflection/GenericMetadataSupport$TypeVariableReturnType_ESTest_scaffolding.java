/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sun May 17 00:01:31 UTC 2020
 */

package org.mockito.internal.util.reflection;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class GenericMetadataSupport$TypeVariableReturnType_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.mockito.internal.util.reflection.GenericMetadataSupport$TypeVariableReturnType"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(GenericMetadataSupport$TypeVariableReturnType_ESTest_scaffolding.class.getClassLoader() ,
      "org.mockito.internal.configuration.plugins.PluginRegistry",
      "org.mockito.internal.util.reflection.GenericMetadataSupport$BoundedType",
      "org.mockito.configuration.AnnotationEngine",
      "org.mockito.cglib.proxy.Callback",
      "org.mockito.internal.util.reflection.GenericMetadataSupport$FromClassGenericMetadataSupport",
      "org.mockito.internal.util.reflection.GenericMetadataSupport$NotGenericReturnTypeSupport",
      "org.mockito.configuration.DefaultMockitoConfiguration",
      "org.mockito.internal.configuration.ClassPathLoader",
      "org.mockito.exceptions.base.MockitoException",
      "org.mockito.plugins.PluginSwitch",
      "org.mockito.stubbing.Answer",
      "org.mockito.internal.util.collections.Iterables",
      "org.mockito.internal.configuration.plugins.PluginLoader",
      "org.mockito.internal.configuration.plugins.DefaultPluginSwitch",
      "org.mockito.internal.exceptions.stacktrace.StackTraceFilter",
      "org.mockito.internal.creation.cglib.CglibMockMaker",
      "org.mockito.configuration.IMockitoConfiguration",
      "org.mockito.internal.util.reflection.GenericMetadataSupport$TypeVariableReturnType",
      "org.mockito.internal.util.Checks",
      "org.mockito.internal.util.reflection.GenericMetadataSupport$FromParameterizedTypeGenericMetadataSupport",
      "org.mockito.internal.exceptions.stacktrace.DefaultStackTraceCleaner",
      "org.mockito.exceptions.stacktrace.StackTraceCleaner",
      "org.mockito.internal.configuration.GlobalConfiguration",
      "org.mockito.internal.configuration.plugins.Plugins",
      "org.mockito.cglib.proxy.MethodInterceptor",
      "org.mockito.internal.util.reflection.GenericMetadataSupport$TypeVarBoundedType",
      "org.mockito.plugins.MockMaker",
      "org.mockito.internal.configuration.plugins.PluginFinder",
      "org.mockito.internal.exceptions.stacktrace.DefaultStackTraceCleanerProvider",
      "org.mockito.plugins.StackTraceCleanerProvider",
      "org.mockito.internal.util.reflection.GenericMetadataSupport",
      "org.mockito.exceptions.misusing.MockitoConfigurationException",
      "org.mockito.internal.util.reflection.GenericMetadataSupport$ParameterizedReturnType",
      "org.mockito.internal.util.reflection.GenericMetadataSupport$WildCardBoundedType",
      "org.mockito.internal.exceptions.stacktrace.ConditionalStackTraceFilter",
      "org.mockito.configuration.MockitoConfiguration"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("java.lang.reflect.Type", false, GenericMetadataSupport$TypeVariableReturnType_ESTest_scaffolding.class.getClassLoader()));
  }
}
