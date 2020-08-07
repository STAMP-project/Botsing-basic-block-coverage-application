/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sat Jan 18 05:13:33 UTC 2020
 */

package com.xpn.xwiki.store;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class XWikiHibernateStore_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "com.xpn.xwiki.store.XWikiHibernateStore"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(XWikiHibernateStore_ESTest_scaffolding.class.getClassLoader() ,
      "com.xpn.xwiki.XWikiException",
      "org.apache.commons.collections4.map.AbstractLinkedMap",
      "org.xwiki.rendering.block.XDOM",
      "org.xwiki.rendering.internal.parser.MissingParserException",
      "org.xwiki.component.phase.Initializable",
      "org.apache.oro.util.GenericCacheEntry",
      "org.suigeneris.jrcs.rcs.Version",
      "org.xwiki.query.Query",
      "org.apache.commons.collections4.KeyValue",
      "com.xpn.xwiki.doc.MetaDataDiff",
      "org.xwiki.model.reference.EntityReference",
      "org.xwiki.component.phase.InitializationException",
      "com.xpn.xwiki.doc.XWikiAttachment",
      "org.apache.commons.lang3.StringUtils",
      "org.apache.oro.text.PatternCacheLRU",
      "com.xpn.xwiki.validation.XWikiValidationStatus",
      "com.xpn.xwiki.util.Util",
      "com.xpn.xwiki.objects.BaseCollection",
      "org.hibernate.Transaction",
      "com.xpn.xwiki.criteria.impl.RevisionCriteria",
      "org.apache.velocity.context.Context",
      "com.xpn.xwiki.web.XWikiForm",
      "org.hibernate.Query",
      "com.xpn.xwiki.store.migration.XWikiDBVersion",
      "org.xwiki.model.EntityType",
      "org.xwiki.model.reference.ObjectReferenceResolver",
      "com.xpn.xwiki.doc.XWikiLink",
      "org.xwiki.rendering.block.MetaDataBlock",
      "com.xpn.xwiki.store.migration.DataMigrationException",
      "com.xpn.xwiki.web.XWikiMessageTool",
      "com.xpn.xwiki.util.AbstractSimpleClass",
      "com.xpn.xwiki.objects.LargeStringProperty",
      "com.xpn.xwiki.objects.FloatProperty",
      "org.apache.struts.action.ActionForm",
      "org.apache.oro.text.PatternCache",
      "com.xpn.xwiki.store.XWikiHibernateStore",
      "org.apache.oro.text.regex.MalformedPatternException",
      "org.hibernate.SessionFactory",
      "org.apache.oro.text.regex.Pattern",
      "org.apache.velocity.context.AbstractContext",
      "com.xpn.xwiki.web.XWikiRequest",
      "org.apache.commons.collections4.map.AbstractHashedMap",
      "org.apache.oro.util.GenericCache",
      "com.xpn.xwiki.plugin.XWikiDefaultPlugin",
      "com.xpn.xwiki.objects.classes.PropertyClass",
      "org.xwiki.query.QueryManager",
      "org.hibernate.SessionException",
      "org.apache.velocity.VelocityContext",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceSerializer",
      "org.xwiki.component.util.DefaultParameterizedType",
      "org.xwiki.context.ExecutionContext",
      "com.xpn.xwiki.store.migration.MigrationRequiredException",
      "org.apache.oro.util.CacheLRU",
      "org.hibernate.ObjectNotFoundException",
      "com.xpn.xwiki.objects.PropertyInterface",
      "org.hibernate.HibernateException",
      "org.xwiki.observation.EventListener",
      "org.apache.velocity.context.InternalContextBase",
      "org.xwiki.context.ExecutionContextException",
      "org.suigeneris.jrcs.diff.DifferentiationFailedException",
      "com.xpn.xwiki.objects.ElementInterface",
      "org.apache.commons.collections4.MapIterator",
      "org.dom4j.Document",
      "org.apache.oro.text.MalformedCachePatternException",
      "org.xwiki.store.UnexpectedException",
      "org.hibernate.Session",
      "com.xpn.xwiki.doc.merge.MergeConfiguration",
      "com.xpn.xwiki.objects.classes.PropertyClassInterface",
      "org.xwiki.rendering.block.Block",
      "com.xpn.xwiki.objects.classes.StringClass",
      "com.xpn.xwiki.doc.AttachmentDiff",
      "com.xpn.xwiki.store.XWikiVersioningStoreInterface",
      "com.xpn.xwiki.user.api.XWikiUser",
      "org.suigeneris.jrcs.util.ToString",
      "com.xpn.xwiki.objects.StringProperty",
      "com.xpn.xwiki.objects.ObjectDiff",
      "com.xpn.xwiki.api.Api",
      "org.xwiki.context.Execution",
      "org.suigeneris.jrcs.diff.DiffException",
      "com.xpn.xwiki.store.XWikiHibernateBaseStore$HibernateCallback",
      "com.xpn.xwiki.objects.ListProperty",
      "com.xpn.xwiki.objects.NumberProperty",
      "com.xpn.xwiki.objects.BaseProperty",
      "com.xpn.xwiki.doc.merge.MergeResult",
      "com.xpn.xwiki.objects.BaseStringProperty",
      "org.hibernate.jdbc.Work",
      "org.xwiki.model.reference.EntityReferenceResolver",
      "org.xwiki.localization.ContextualLocalizationManager",
      "org.xwiki.model.reference.DocumentReference",
      "com.xpn.xwiki.XWikiContext",
      "org.xwiki.observation.event.Event",
      "com.xpn.xwiki.doc.XWikiDocument",
      "org.apache.commons.collections4.map.AbstractHashedMap$HashEntry",
      "org.xwiki.model.reference.WikiReference",
      "com.xpn.xwiki.objects.classes.BaseClass",
      "org.xwiki.rendering.parser.ParseException",
      "org.xwiki.display.internal.DocumentDisplayer",
      "org.apache.velocity.context.InternalEventContext",
      "com.xpn.xwiki.internal.render.OldRendering",
      "org.xwiki.bridge.DocumentModelBridge",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceSerializer",
      "org.dom4j.Node",
      "org.xwiki.rendering.block.HeaderBlock",
      "org.apache.commons.collections4.map.LRUMap",
      "com.xpn.xwiki.store.XWikiStoreInterface",
      "org.xwiki.model.reference.SpaceReference",
      "com.xpn.xwiki.objects.classes.ClassInterface",
      "org.apache.commons.collections4.Get",
      "com.xpn.xwiki.objects.ObjectInterface",
      "org.apache.velocity.context.InternalHousekeepingContext",
      "org.xwiki.rendering.syntax.Syntax",
      "org.xwiki.component.annotation.Role",
      "com.xpn.xwiki.plugin.XWikiPluginInterface",
      "org.xwiki.model.internal.reference.LocalStringEntityReferenceSerializer",
      "org.hibernate.cfg.Configuration",
      "com.xpn.xwiki.web.XWikiResponse",
      "org.xwiki.rendering.transformation.TransformationContext",
      "org.apache.commons.collections4.OrderedMap",
      "org.xwiki.model.reference.EntityReferenceSerializer",
      "org.xwiki.model.internal.reference.LocalizedStringEntityReferenceSerializer",
      "com.xpn.xwiki.store.XWikiAttachmentStoreInterface",
      "com.xpn.xwiki.monitor.api.MonitorPlugin",
      "com.xpn.xwiki.store.migration.DataMigrationManager",
      "org.xwiki.logging.LoggerManager",
      "org.apache.oro.util.Cache",
      "com.xpn.xwiki.doc.XWikiDocument$3",
      "com.xpn.xwiki.api.Document",
      "org.apache.commons.collections4.OrderedMapIterator",
      "org.xwiki.rendering.transformation.TransformationException",
      "org.xwiki.model.reference.ObjectPropertyReference",
      "org.apache.oro.text.regex.PatternCompiler",
      "com.xpn.xwiki.store.hibernate.HibernateSessionFactory",
      "org.apache.commons.collections4.IterableMap",
      "org.suigeneris.jrcs.diff.delta.Delta",
      "com.xpn.xwiki.internal.xml.DOMXMLWriter",
      "org.apache.commons.collections4.IterableGet",
      "com.xpn.xwiki.doc.XWikiDocument$XWikiAttachmentToRemove",
      "com.xpn.xwiki.XWiki",
      "org.xwiki.observation.ObservationManager",
      "org.xwiki.rendering.block.AbstractBlock",
      "com.xpn.xwiki.store.DatabaseProduct",
      "com.xpn.xwiki.internal.xml.XMLWriter",
      "org.xwiki.query.QueryException",
      "org.xwiki.component.annotation.Component",
      "org.dom4j.io.XMLWriter",
      "org.xwiki.rendering.listener.reference.ResourceReference",
      "com.xpn.xwiki.doc.XWikiDocumentArchive",
      "com.xpn.xwiki.web.EditForm",
      "com.xpn.xwiki.web.XWikiURLFactory",
      "org.hibernate.UnresolvableObjectException",
      "org.apache.oro.text.regex.Perl5Compiler",
      "org.xwiki.model.reference.DocumentReferenceResolver",
      "org.dom4j.DocumentException",
      "org.xwiki.logging.LogLevel",
      "org.suigeneris.jrcs.diff.Revision",
      "org.xwiki.rendering.transformation.RenderingContext",
      "com.xpn.xwiki.store.migration.DataMigrationStatus",
      "org.xwiki.model.reference.LocalDocumentReference",
      "com.xpn.xwiki.web.XWikiEngineContext",
      "org.xwiki.rendering.renderer.printer.WikiPrinter",
      "com.xpn.xwiki.api.DocumentSection",
      "org.xwiki.rendering.block.match.BlockMatcher",
      "com.xpn.xwiki.internal.cache.rendering.RenderingCache",
      "com.xpn.xwiki.objects.BaseObject",
      "org.apache.oro.text.GenericPatternCache",
      "org.dom4j.Branch",
      "com.xpn.xwiki.doc.rcs.XWikiRCSNodeInfo",
      "org.hibernate.FlushMode",
      "org.apache.commons.collections4.BoundedMap",
      "com.xpn.xwiki.objects.BaseElement",
      "com.xpn.xwiki.doc.XWikiLock",
      "org.xwiki.rendering.syntax.SyntaxFactory",
      "org.apache.commons.collections4.Put",
      "org.xwiki.model.reference.ObjectReference",
      "com.xpn.xwiki.store.XWikiHibernateBaseStore",
      "org.xwiki.job.event.status.JobProgressManager",
      "org.dom4j.Element",
      "org.apache.commons.collections4.map.AbstractLinkedMap$LinkEntry",
      "org.xwiki.display.internal.Displayer",
      "org.apache.commons.collections4.OrderedIterator",
      "com.xpn.xwiki.doc.XWikiSpace"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("com.xpn.xwiki.doc.XWikiDocument", false, XWikiHibernateStore_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("com.xpn.xwiki.store.hibernate.HibernateSessionFactory", false, XWikiHibernateStore_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("com.xpn.xwiki.store.migration.DataMigrationManager", false, XWikiHibernateStore_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("javax.inject.Provider", false, XWikiHibernateStore_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.slf4j.Logger", false, XWikiHibernateStore_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.context.Execution", false, XWikiHibernateStore_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.logging.LoggerManager", false, XWikiHibernateStore_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.model.reference.DocumentReferenceResolver", false, XWikiHibernateStore_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.model.reference.EntityReferenceSerializer", false, XWikiHibernateStore_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.observation.ObservationManager", false, XWikiHibernateStore_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.xwiki.query.QueryManager", false, XWikiHibernateStore_ESTest_scaffolding.class.getClassLoader()));
  }
}
