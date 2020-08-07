/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sat May 16 14:39:09 UTC 2020
 */

package org.xwiki.search.solr.internal.reference;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class SolrEntityReferenceResolver_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.search.solr.internal.reference.SolrEntityReferenceResolver"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(SolrEntityReferenceResolver_ESTest_scaffolding.class.getClassLoader() ,
      "org.xwiki.model.internal.reference.DeprecatedExplicitReferenceEntityReferenceResolver2",
      "org.aspectj.lang.reflect.AjType",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceSerializer",
      "org.apache.solr.common.SolrDocument$2",
      "org.xwiki.model.reference.EntityReference",
      "org.apache.solr.common.SolrDocument$1",
      "org.xwiki.model.internal.reference.DeprecatedExplicitStringEntityReferenceResolver",
      "org.apache.commons.lang3.StringUtils",
      "org.xwiki.component.annotation.Role",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceResolver",
      "org.xwiki.model.EntityType",
      "org.xwiki.model.reference.EntityReferenceSerializer",
      "org.xwiki.model.reference.InvalidEntityReferenceException",
      "com.google.inject.internal.MoreTypes$GenericArrayTypeImpl",
      "org.xwiki.model.internal.reference.LocalizedStringEntityReferenceSerializer",
      "com.google.gson.internal.$Gson$Types$WildcardTypeImpl",
      "org.xwiki.model.internal.reference.DeprecatedDefaultReferenceEntityReferenceResolver2",
      "org.xwiki.model.internal.reference.StringReferenceSeparators",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceResolver",
      "com.google.inject.internal.MoreTypes$WildcardTypeImpl",
      "org.xwiki.model.internal.reference.AbstractReferenceEntityReferenceResolver$1",
      "org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceSerializer",
      "org.xwiki.component.util.DefaultParameterizedType",
      "com.google.common.reflect.Types$WildcardTypeImpl",
      "org.xwiki.model.internal.reference.DefaultReferenceEntityReferenceResolver",
      "com.google.gson.internal.$Gson$Types$GenericArrayTypeImpl",
      "org.xwiki.model.internal.reference.DeprecatedExplicitReferenceEntityReferenceResolver",
      "org.xwiki.stability.Unstable",
      "org.xwiki.component.annotation.Component",
      "org.xwiki.text.StringUtils",
      "org.xwiki.search.solr.internal.reference.SolrEntityReferenceResolver",
      "com.fasterxml.jackson.core.type.ResolvedType",
      "org.xwiki.model.internal.reference.AbstractReferenceEntityReferenceResolver",
      "org.xwiki.model.internal.reference.ExplicitReferenceEntityReferenceResolver",
      "com.google.inject.internal.MoreTypes$CompositeType",
      "com.google.inject.internal.MoreTypes",
      "com.google.common.reflect.Types",
      "org.xwiki.search.solr.internal.reference.SolrEntityReferenceResolver$1",
      "org.xwiki.model.internal.reference.RelativeStringEntityReferenceResolver",
      "org.xwiki.model.reference.LocalDocumentReference",
      "com.google.common.reflect.Types$NativeTypeVariableEquals",
      "com.fasterxml.jackson.databind.JavaType",
      "org.xwiki.model.reference.EntityReferenceProvider",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$3",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$4",
      "com.google.common.reflect.Types$GenericArrayTypeImpl",
      "com.google.common.reflect.Types$JavaVersion",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$1",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$2",
      "org.xwiki.model.internal.reference.DeprecatedDefaultReferenceEntityReferenceResolver",
      "org.xwiki.model.internal.reference.DeprecatedDefaultStringEntityReferenceResolver",
      "org.apache.solr.common.util.NamedList",
      "org.xwiki.model.reference.EntityReferenceResolver",
      "org.apache.solr.common.SolrDocument",
      "com.google.inject.internal.MoreTypes$ParameterizedTypeImpl",
      "org.xwiki.model.reference.DocumentReference",
      "org.xwiki.model.internal.reference.DeprecatedRelativeStringEntityReferenceResolver",
      "com.google.gson.internal.$Gson$Types$ParameterizedTypeImpl",
      "org.xwiki.model.internal.reference.AbstractEntityReferenceResolver",
      "com.google.common.reflect.Types$ParameterizedTypeImpl"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.xwiki.model.reference.EntityReferenceProvider", false, SolrEntityReferenceResolver_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.model.reference.EntityReferenceResolver", false, SolrEntityReferenceResolver_ESTest_scaffolding.class.getClassLoader()));
  }
}
