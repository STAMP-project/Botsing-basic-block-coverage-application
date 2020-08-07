/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Wed Apr 01 03:07:06 UTC 2020
 */

package org.xwiki.rest.internal;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class DomainObjectFactory_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.rest.internal.DomainObjectFactory"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(DomainObjectFactory_ESTest_scaffolding.class.getClassLoader() ,
      "com.xpn.xwiki.XWikiException",
      "org.xwiki.component.phase.Initializable",
      "org.suigeneris.jrcs.rcs.Version",
      "org.xwiki.job.event.status.JobStatus",
      "org.xwiki.logging.marker.AbstractContainerMarker",
      "org.xwiki.job.DefaultRequest",
      "org.xwiki.job.event.status.StepProgressEvent",
      "org.xwiki.component.phase.InitializationException",
      "org.apache.commons.lang3.StringUtils",
      "com.xpn.xwiki.objects.BaseCollection",
      "org.hibernate.loader.custom.sql.SQLCustomQuery",
      "org.xwiki.rest.model.jaxb.PageSummary",
      "org.xwiki.rest.model.jaxb.Link",
      "org.xwiki.rest.model.jaxb.Objects",
      "org.xwiki.logging.marker.TranslationMarker",
      "org.xwiki.component.manager.CompatibilityComponentManager",
      "org.xwiki.logging.marker.ContainerMarker",
      "org.xwiki.rest.model.jaxb.ObjectSummary",
      "org.xwiki.rest.model.jaxb.Xwiki",
      "org.xwiki.rest.model.jaxb.Translation",
      "org.xwiki.job.DefaultJobStatus",
      "org.xwiki.job.internal.DefaultJobProgress",
      "com.xpn.xwiki.api.XWiki",
      "org.xwiki.observation.EventListener",
      "org.xwiki.job.event.status.AbstractProgressEvent",
      "com.xpn.xwiki.objects.ElementInterface",
      "org.xwiki.rest.model.jaxb.JobProgress",
      "org.xwiki.rest.model.jaxb.Attribute",
      "org.xwiki.job.Request",
      "org.xwiki.component.manager.ComponentLookupException",
      "org.xwiki.job.event.status.StartStepProgressEvent",
      "org.xwiki.component.manager.ComponentRepositoryException",
      "org.xwiki.observation.internal.DefaultObservationManager",
      "org.xwiki.rest.model.jaxb.Property",
      "org.suigeneris.jrcs.util.ToString",
      "com.xpn.xwiki.api.Api",
      "org.xwiki.logging.logback.internal.LogbackUtils",
      "org.xwiki.rest.model.jaxb.Tag",
      "org.xwiki.logging.logback.internal.DefaultLoggerManager",
      "org.xwiki.rest.model.jaxb.Class",
      "org.xwiki.rest.model.jaxb.JobStatus",
      "org.xwiki.job.event.status.JobStatus$State",
      "org.xwiki.observation.event.Event",
      "com.xpn.xwiki.XWikiContext",
      "org.xwiki.rest.model.jaxb.Object",
      "org.xwiki.rest.model.jaxb.Tags",
      "org.xwiki.component.descriptor.ComponentRole",
      "org.xwiki.rest.model.jaxb.Pages",
      "org.xwiki.rest.model.jaxb.SearchResult",
      "org.xwiki.job.annotation.Serializable",
      "org.xwiki.rest.model.jaxb.Attachments",
      "org.xwiki.component.manager.ComponentLifecycleException",
      "org.xwiki.rest.model.jaxb.Translations",
      "com.xpn.xwiki.objects.ObjectInterface",
      "org.xwiki.component.annotation.Role",
      "org.hibernate.loader.custom.CustomQuery",
      "org.xwiki.job.event.status.PushLevelProgressEvent",
      "org.xwiki.component.manager.ComponentEventManager",
      "org.xwiki.component.descriptor.ComponentDescriptor",
      "org.xwiki.rest.model.jaxb.Comments",
      "org.xwiki.rest.model.jaxb.History",
      "org.hibernate.loader.custom.sql.SQLQueryParser$ParserContext",
      "org.xwiki.rest.model.jaxb.HistorySummary",
      "org.xwiki.rest.XWikiRestException",
      "org.xwiki.rest.model.jaxb.JobLog",
      "org.xwiki.logging.LoggerManager",
      "org.xwiki.job.event.status.JobProgress",
      "com.xpn.xwiki.api.Document",
      "com.xpn.xwiki.api.Class",
      "org.xwiki.rest.model.jaxb.LinkCollection",
      "org.xwiki.job.event.status.PopLevelProgressEvent",
      "com.xpn.xwiki.api.Attachment",
      "org.xwiki.component.manager.ComponentManager",
      "org.xwiki.rest.model.jaxb.Attachment",
      "org.xwiki.rest.model.jaxb.Syntaxes",
      "org.xwiki.job.event.status.JobProgressStep",
      "org.xwiki.logging.logback.internal.ForbiddenThreadsFilter",
      "org.xwiki.logging.Message",
      "com.xpn.xwiki.api.Element",
      "org.xwiki.observation.ObservationManager",
      "org.xwiki.rest.model.jaxb.Wiki",
      "org.xwiki.component.annotation.Component",
      "com.xpn.xwiki.api.Collection",
      "org.xwiki.rest.model.jaxb.ObjectFactory",
      "org.xwiki.job.AbstractJobStatus",
      "org.xwiki.logging.LogLevel",
      "org.xwiki.job.event.status.EndStepProgressEvent",
      "org.xwiki.job.internal.DefaultJobProgressStep",
      "org.xwiki.rest.model.jaxb.Properties",
      "org.xwiki.logging.Logger",
      "org.xwiki.rest.model.jaxb.Space",
      "com.xpn.xwiki.objects.BaseObject",
      "org.xwiki.rest.model.jaxb.Wikis",
      "org.xwiki.rest.model.jaxb.LogEvent",
      "org.xwiki.rest.model.jaxb.Page",
      "org.xwiki.job.AbstractRequest",
      "org.xwiki.rest.internal.ModelFactory",
      "com.xpn.xwiki.api.Object",
      "org.xwiki.rest.internal.DomainObjectFactory",
      "com.xpn.xwiki.objects.BaseElement",
      "org.xwiki.rest.model.jaxb.SearchResults",
      "com.xpn.xwiki.objects.CompatibilityObjectInterface",
      "org.xwiki.rest.model.jaxb.Spaces",
      "org.xwiki.logging.LogQueue",
      "org.xwiki.rest.model.jaxb.Comment",
      "org.xwiki.job.event.status.ProgressEvent",
      "org.xwiki.rest.model.jaxb.Classes"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.xwiki.component.manager.ComponentManager", false, DomainObjectFactory_ESTest_scaffolding.class.getClassLoader()));
  }
}
