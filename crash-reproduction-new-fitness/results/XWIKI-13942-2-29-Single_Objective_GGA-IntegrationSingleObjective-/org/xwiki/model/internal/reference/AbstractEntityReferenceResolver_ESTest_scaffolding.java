/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sun May 17 17:19:17 UTC 2020
 */

package org.xwiki.model.internal.reference;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class AbstractEntityReferenceResolver_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.model.internal.reference.AbstractEntityReferenceResolver"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(AbstractEntityReferenceResolver_ESTest_scaffolding.class.getClassLoader() ,
      "org.xwiki.component.phase.Initializable",
      "org.aspectj.lang.reflect.AjType",
      "org.xwiki.stability.Unstable",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceSerializer",
      "org.xwiki.component.annotation.Component",
      "org.xwiki.model.reference.EntityReference",
      "org.xwiki.model.internal.reference.DefaultSymbolScheme$1",
      "org.xwiki.component.phase.InitializationException",
      "org.xwiki.model.internal.reference.DefaultSymbolScheme",
      "com.fasterxml.jackson.core.type.ResolvedType",
      "org.xwiki.model.internal.reference.AbstractReferenceEntityReferenceResolver",
      "org.xwiki.model.internal.reference.ExplicitReferenceEntityReferenceResolver",
      "com.google.inject.internal.MoreTypes$CompositeType",
      "com.google.inject.internal.MoreTypes",
      "org.xwiki.component.annotation.Role",
      "com.google.common.reflect.Types",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceResolver",
      "com.fasterxml.jackson.databind.type.TypeBindings",
      "org.xwiki.model.internal.reference.RelativeStringEntityReferenceResolver",
      "com.google.common.reflect.Types$NativeTypeVariableEquals",
      "com.fasterxml.jackson.databind.JavaType",
      "org.xwiki.model.EntityType",
      "org.xwiki.model.reference.EntityReferenceSerializer",
      "org.xwiki.model.reference.InvalidEntityReferenceException",
      "com.google.inject.internal.MoreTypes$GenericArrayTypeImpl",
      "org.xwiki.model.internal.reference.LocalizedStringEntityReferenceSerializer",
      "org.xwiki.model.reference.EntityReferenceProvider",
      "com.google.common.reflect.Types$GenericArrayTypeImpl",
      "com.google.gson.internal.$Gson$Types$WildcardTypeImpl",
      "org.xwiki.model.internal.reference.SymbolScheme",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceResolver",
      "com.google.inject.internal.MoreTypes$WildcardTypeImpl",
      "com.google.common.reflect.Types$JavaVersion",
      "org.xwiki.model.reference.EntityReferenceResolver",
      "org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceSerializer",
      "org.xwiki.component.util.DefaultParameterizedType",
      "com.google.inject.internal.MoreTypes$ParameterizedTypeImpl",
      "com.google.gson.internal.$Gson$Types$ParameterizedTypeImpl",
      "com.google.common.reflect.Types$WildcardTypeImpl",
      "org.xwiki.model.internal.reference.AbstractEntityReferenceResolver",
      "org.xwiki.model.internal.reference.DefaultReferenceEntityReferenceResolver",
      "com.google.common.reflect.Types$ParameterizedTypeImpl",
      "com.google.gson.internal.$Gson$Types$GenericArrayTypeImpl"
    );
  } 
}
