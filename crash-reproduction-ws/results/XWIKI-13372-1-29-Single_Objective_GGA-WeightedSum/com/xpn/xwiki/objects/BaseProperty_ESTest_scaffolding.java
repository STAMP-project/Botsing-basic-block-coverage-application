/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Tue Mar 31 08:56:30 UTC 2020
 */

package com.xpn.xwiki.objects;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class BaseProperty_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "com.xpn.xwiki.objects.BaseProperty"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(BaseProperty_ESTest_scaffolding.class.getClassLoader() ,
      "com.xpn.xwiki.XWikiException",
      "org.dom4j.tree.DefaultElement",
      "org.xwiki.bridge.DocumentModelBridge",
      "org.dom4j.Node",
      "org.xwiki.model.reference.EntityReference",
      "org.dom4j.util.SingletonStrategy",
      "com.xpn.xwiki.objects.StringListProperty",
      "org.dom4j.tree.DefaultText",
      "com.xpn.xwiki.objects.classes.ClassInterface",
      "org.apache.commons.lang3.StringUtils",
      "com.xpn.xwiki.objects.ObjectInterface",
      "com.xpn.xwiki.objects.BaseCollection",
      "org.dom4j.Entity",
      "org.dom4j.tree.NamespaceStack",
      "com.xpn.xwiki.web.Utils",
      "org.dom4j.DocumentType",
      "org.dom4j.tree.AbstractBranch",
      "org.dom4j.tree.DefaultDocument",
      "org.xwiki.model.reference.EntityReferenceSerializer",
      "org.dom4j.dom.DOMElement",
      "org.dom4j.ProcessingInstruction",
      "com.xpn.xwiki.objects.LargeStringProperty",
      "com.xpn.xwiki.objects.FloatProperty",
      "org.dom4j.Namespace",
      "com.xpn.xwiki.objects.BaseObjectReference",
      "org.dom4j.tree.AbstractText",
      "org.xwiki.model.reference.ObjectPropertyReference",
      "com.xpn.xwiki.objects.DoubleProperty",
      "org.xwiki.component.util.DefaultParameterizedType",
      "org.xwiki.component.manager.ComponentManager",
      "org.dom4j.tree.AbstractDocument",
      "org.dom4j.util.SimpleSingleton",
      "org.dom4j.tree.NamespaceCache",
      "com.xpn.xwiki.objects.PropertyInterface",
      "com.xpn.xwiki.api.Element",
      "org.dom4j.dom.DOMDocument",
      "org.dom4j.Attribute",
      "com.xpn.xwiki.objects.ElementInterface",
      "org.dom4j.Document",
      "org.dom4j.tree.ConcurrentReaderHashMap",
      "org.dom4j.tree.ConcurrentReaderHashMap$Entry",
      "org.dom4j.io.XMLWriter",
      "org.dom4j.io.OutputFormat",
      "org.dom4j.dom.DOMNamespace",
      "org.xwiki.component.manager.ComponentLookupException",
      "com.xpn.xwiki.api.Collection",
      "org.dom4j.tree.QNameCache",
      "org.dom4j.tree.AbstractElement",
      "com.xpn.xwiki.objects.LongProperty",
      "com.xpn.xwiki.doc.merge.MergeConfiguration",
      "org.dom4j.DocumentFactory",
      "org.dom4j.CDATA",
      "org.dom4j.IllegalAddException",
      "org.dom4j.rule.Pattern",
      "org.dom4j.Comment",
      "org.dom4j.tree.FlyweightText",
      "com.xpn.xwiki.objects.StringProperty",
      "org.dom4j.tree.AbstractNode",
      "com.xpn.xwiki.api.Api",
      "org.dom4j.tree.ConcurrentReaderHashMap$BarrierLock",
      "com.xpn.xwiki.objects.ListProperty",
      "org.dom4j.CharacterData",
      "org.dom4j.dom.DOMDocumentFactory",
      "com.xpn.xwiki.objects.DBStringListProperty",
      "com.xpn.xwiki.objects.NumberProperty",
      "org.dom4j.NodeFilter",
      "org.dom4j.tree.DefaultNamespace",
      "com.xpn.xwiki.objects.BaseObject",
      "com.xpn.xwiki.objects.BaseProperty",
      "org.dom4j.Branch",
      "com.xpn.xwiki.doc.merge.MergeResult",
      "com.xpn.xwiki.api.Object",
      "org.dom4j.Text",
      "org.dom4j.tree.AbstractCharacterData",
      "com.xpn.xwiki.objects.BaseElement",
      "com.xpn.xwiki.objects.BaseStringProperty",
      "org.xwiki.localization.ContextualLocalizationManager",
      "org.xwiki.model.reference.ObjectReference",
      "org.dom4j.XPath",
      "org.xwiki.model.reference.DocumentReference",
      "com.xpn.xwiki.objects.IntegerProperty",
      "com.xpn.xwiki.XWikiContext",
      "org.dom4j.Element",
      "com.xpn.xwiki.doc.XWikiDocument",
      "org.dom4j.QName",
      "com.xpn.xwiki.objects.ElementComparator",
      "com.xpn.xwiki.objects.classes.BaseClass"
    );
  } 
}
