/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sat May 16 13:37:52 UTC 2020
 */

package org.xwiki.mail.internal.configuration;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class SendMailConfigClassDocumentConfigurationSource_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.mail.internal.configuration.SendMailConfigClassDocumentConfigurationSource"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(SendMailConfigClassDocumentConfigurationSource_ESTest_scaffolding.class.getClassLoader() ,
      "com.xpn.xwiki.XWikiException",
      "org.xwiki.component.phase.Initializable",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceSerializer",
      "org.xwiki.model.reference.EntityReference",
      "org.xwiki.model.reference.SpaceReference",
      "org.xwiki.component.phase.Disposable",
      "org.xwiki.component.phase.InitializationException",
      "org.xwiki.component.manager.ComponentLifecycleException",
      "org.apache.commons.lang3.StringUtils",
      "com.xpn.xwiki.objects.ObjectInterface",
      "com.xpn.xwiki.objects.BaseCollection",
      "org.xwiki.component.annotation.Role",
      "org.xwiki.configuration.internal.AbstractDocumentConfigurationSource$1",
      "org.xwiki.cache.CacheFactory",
      "org.xwiki.model.EntityType",
      "org.xwiki.model.reference.EntityReferenceSerializer",
      "org.xwiki.model.internal.reference.LocalizedStringEntityReferenceSerializer",
      "org.xwiki.cache.config.CacheConfiguration",
      "org.xwiki.model.internal.reference.StringReferenceSeparators",
      "org.xwiki.wiki.manager.WikiManagerException",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceSerializer",
      "org.xwiki.component.util.DefaultParameterizedType",
      "org.xwiki.observation.EventListener",
      "com.xpn.xwiki.objects.ElementInterface",
      "org.xwiki.wiki.descriptor.WikiDescriptorManager",
      "org.xwiki.observation.ObservationManager",
      "org.xwiki.component.annotation.Component",
      "org.xwiki.properties.ConverterManager",
      "org.xwiki.text.StringUtils",
      "org.xwiki.component.manager.ComponentLookupException",
      "org.xwiki.mail.internal.configuration.SendMailConfigClassDocumentConfigurationSource",
      "org.xwiki.model.reference.LocalDocumentReference",
      "org.xwiki.configuration.internal.AbstractConfigurationSource",
      "org.xwiki.cache.CacheManager",
      "org.xwiki.model.reference.RegexEntityReference",
      "com.xpn.xwiki.objects.BaseObject",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$3",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$4",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$1",
      "org.xwiki.model.internal.reference.StringReferenceSeparators$2",
      "org.xwiki.model.reference.PartialEntityReference",
      "com.xpn.xwiki.objects.BaseElement",
      "org.xwiki.wiki.descriptor.WikiDescriptor",
      "org.xwiki.configuration.internal.AbstractDocumentConfigurationSource",
      "org.xwiki.model.reference.DocumentReference",
      "org.xwiki.observation.event.Event",
      "com.xpn.xwiki.XWikiContext",
      "org.xwiki.configuration.ConfigurationSource",
      "org.xwiki.model.reference.WikiReference",
      "org.xwiki.cache.CacheException",
      "org.xwiki.cache.Cache"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("javax.inject.Provider", false, SendMailConfigClassDocumentConfigurationSource_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.slf4j.Logger", false, SendMailConfigClassDocumentConfigurationSource_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.cache.CacheManager", false, SendMailConfigClassDocumentConfigurationSource_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.model.reference.EntityReferenceSerializer", false, SendMailConfigClassDocumentConfigurationSource_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.observation.ObservationManager", false, SendMailConfigClassDocumentConfigurationSource_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.properties.ConverterManager", false, SendMailConfigClassDocumentConfigurationSource_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.wiki.descriptor.WikiDescriptorManager", false, SendMailConfigClassDocumentConfigurationSource_ESTest_scaffolding.class.getClassLoader()));
  }
}
