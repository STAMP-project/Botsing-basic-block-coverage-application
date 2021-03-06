/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Tue Mar 31 02:05:06 UTC 2020
 */

package com.xpn.xwiki.doc;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class XWikiDocument_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "com.xpn.xwiki.doc.XWikiDocument"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(XWikiDocument_ESTest_scaffolding.class.getClassLoader() ,
      "com.xpn.xwiki.XWikiException",
      "org.apache.commons.collections4.map.AbstractLinkedMap",
      "org.xwiki.rendering.block.XDOM",
      "org.suigeneris.jrcs.rcs.Version",
      "org.apache.commons.collections4.KeyValue",
      "org.xwiki.model.reference.EntityReference",
      "org.apache.commons.lang3.StringUtils",
      "com.xpn.xwiki.doc.XWikiAttachment",
      "com.xpn.xwiki.validation.XWikiValidationStatus",
      "com.xpn.xwiki.util.Util",
      "com.xpn.xwiki.objects.BaseCollection",
      "com.xpn.xwiki.criteria.impl.RevisionCriteria",
      "org.apache.velocity.context.Context",
      "com.xpn.xwiki.internal.AbstractNotifyOnUpdateList",
      "org.xwiki.component.manager.CompatibilityComponentManager",
      "com.xpn.xwiki.web.XWikiForm",
      "org.xwiki.model.EntityType",
      "org.xwiki.model.reference.ObjectReferenceResolver",
      "org.xwiki.rendering.block.MetaDataBlock",
      "org.xwiki.filter.output.OutputTarget",
      "com.xpn.xwiki.web.XWikiMessageTool",
      "com.xpn.xwiki.util.AbstractSimpleClass",
      "org.apache.struts.action.ActionForm",
      "org.xwiki.model.internal.reference.SymbolScheme",
      "com.xpn.xwiki.web.XWikiRequest",
      "org.apache.velocity.context.AbstractContext",
      "org.apache.commons.collections4.map.AbstractHashedMap",
      "com.xpn.xwiki.objects.classes.PropertyClass",
      "org.apache.velocity.VelocityContext",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceSerializer",
      "org.xwiki.component.util.DefaultParameterizedType",
      "org.xwiki.context.ExecutionContext",
      "com.xpn.xwiki.objects.PropertyInterface",
      "org.apache.velocity.context.InternalContextBase",
      "org.xwiki.observation.EventListener",
      "org.xwiki.context.ExecutionContextException",
      "org.suigeneris.jrcs.diff.DifferentiationFailedException",
      "com.xpn.xwiki.objects.ElementInterface",
      "org.apache.commons.collections4.MapIterator",
      "org.dom4j.Document",
      "org.xwiki.component.manager.ComponentLookupException",
      "com.xpn.xwiki.doc.merge.MergeConfiguration",
      "org.xwiki.rendering.block.Block",
      "com.xpn.xwiki.objects.classes.PropertyClassInterface",
      "org.xwiki.rendering.parser.MissingParserException",
      "com.xpn.xwiki.store.XWikiVersioningStoreInterface",
      "com.xpn.xwiki.user.api.XWikiUser",
      "org.suigeneris.jrcs.util.ToString",
      "com.xpn.xwiki.api.Api",
      "org.suigeneris.jrcs.diff.DiffException",
      "com.xpn.xwiki.objects.BaseProperty",
      "com.xpn.xwiki.doc.merge.MergeResult",
      "org.xwiki.model.reference.EntityReferenceResolver",
      "org.xwiki.localization.ContextualLocalizationManager",
      "org.xwiki.model.reference.DocumentReference",
      "com.xpn.xwiki.XWikiContext",
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
      "org.apache.commons.collections4.map.LRUMap",
      "org.xwiki.model.reference.SpaceReference",
      "com.xpn.xwiki.store.XWikiStoreInterface",
      "org.xwiki.model.internal.reference.DefaultSymbolScheme",
      "com.xpn.xwiki.objects.classes.ClassInterface",
      "org.apache.commons.collections4.Get",
      "com.xpn.xwiki.objects.ObjectInterface",
      "org.apache.velocity.context.InternalHousekeepingContext",
      "org.xwiki.rendering.syntax.Syntax",
      "com.xpn.xwiki.web.Utils",
      "org.xwiki.model.internal.reference.LocalStringEntityReferenceSerializer",
      "com.xpn.xwiki.web.XWikiResponse",
      "org.xwiki.rendering.transformation.TransformationContext",
      "org.apache.commons.collections4.OrderedMap",
      "org.xwiki.model.reference.EntityReferenceSerializer",
      "org.xwiki.model.internal.reference.LocalizedStringEntityReferenceSerializer",
      "com.xpn.xwiki.store.XWikiAttachmentStoreInterface",
      "com.xpn.xwiki.api.Document",
      "org.apache.commons.collections4.OrderedMapIterator",
      "org.xwiki.rendering.transformation.TransformationException",
      "org.xwiki.model.reference.ObjectPropertyReference",
      "com.xpn.xwiki.doc.XWikiDocument$1",
      "org.xwiki.component.manager.ComponentManager",
      "org.apache.commons.collections4.IterableMap",
      "org.xwiki.filter.input.InputSource",
      "com.xpn.xwiki.internal.xml.DOMXMLWriter",
      "org.apache.commons.collections4.IterableGet",
      "com.xpn.xwiki.XWiki",
      "org.xwiki.rendering.block.AbstractBlock",
      "com.xpn.xwiki.internal.xml.XMLWriter",
      "org.xwiki.query.QueryException",
      "org.dom4j.io.XMLWriter",
      "org.xwiki.model.internal.reference.DefaultSymbolScheme$1",
      "com.xpn.xwiki.doc.XWikiDocumentArchive",
      "com.xpn.xwiki.web.EditForm",
      "com.xpn.xwiki.web.XWikiURLFactory",
      "org.xwiki.model.reference.DocumentReferenceResolver",
      "org.suigeneris.jrcs.diff.Revision",
      "org.xwiki.rendering.transformation.RenderingContext",
      "org.xwiki.model.reference.LocalDocumentReference",
      "com.xpn.xwiki.web.XWikiEngineContext",
      "org.xwiki.rendering.renderer.printer.WikiPrinter",
      "com.xpn.xwiki.api.DocumentSection",
      "org.xwiki.rendering.block.match.BlockMatcher",
      "com.xpn.xwiki.internal.cache.rendering.RenderingCache",
      "com.xpn.xwiki.objects.BaseObject",
      "org.dom4j.Branch",
      "com.xpn.xwiki.doc.rcs.XWikiRCSNodeInfo",
      "org.apache.commons.collections4.BoundedMap",
      "com.xpn.xwiki.objects.BaseElement",
      "com.xpn.xwiki.doc.XWikiLock",
      "org.xwiki.rendering.syntax.SyntaxFactory",
      "org.apache.commons.collections4.Put",
      "org.xwiki.model.reference.ObjectReference",
      "org.xwiki.job.event.status.JobProgressManager",
      "org.dom4j.Element",
      "org.apache.commons.collections4.map.AbstractLinkedMap$LinkEntry",
      "org.xwiki.display.internal.Displayer",
      "org.apache.commons.collections4.OrderedIterator"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.xwiki.model.reference.DocumentReference", false, XWikiDocument_ESTest_scaffolding.class.getClassLoader()));
  }
}
