/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Thu May 14 14:04:28 UTC 2020
 */

package com.xpn.xwiki.objects;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class BaseStringProperty_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "com.xpn.xwiki.objects.BaseStringProperty"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(BaseStringProperty_ESTest_scaffolding.class.getClassLoader() ,
      "com.xpn.xwiki.XWikiException",
      "org.apache.commons.collections4.map.AbstractLinkedMap",
      "org.xwiki.rendering.block.XDOM",
      "org.apache.commons.collections4.KeyValue",
      "org.xwiki.model.internal.reference.ExplicitReferenceDocumentReferenceResolver",
      "org.infinispan.commons.marshall.MarshallableFunctions$Remove",
      "org.infinispan.stream.impl.TerminalFunctions$MaxLongFunction",
      "org.dom4j.datatype.DatatypeAttribute",
      "org.infinispan.stream.StreamMarshalling$EqualityPredicate",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueReturnPrevOrNull",
      "org.apache.commons.lang3.StringUtils",
      "org.dom4j.Entity",
      "org.dom4j.tree.NamespaceStack",
      "org.infinispan.iteration.impl.LocalEntryRetriever$MapAction",
      "org.infinispan.commons.marshall.MarshallableFunctions$RemoveReturnPrevOrNull",
      "org.xwiki.component.manager.CompatibilityComponentManager",
      "org.dom4j.tree.AbstractBranch",
      "org.dom4j.ProcessingInstruction",
      "org.infinispan.stream.impl.DistributedCacheStream",
      "org.xwiki.xml.XMLUtils",
      "org.infinispan.stream.impl.TerminalFunctions$SumIntFunction",
      "org.dom4j.tree.FlyweightProcessingInstruction",
      "org.dom4j.Namespace",
      "org.dom4j.datatype.DatatypeElement",
      "org.dom4j.tree.DefaultDocumentType",
      "org.xwiki.model.internal.reference.DeprecatedDefaultReferenceEntityReferenceResolver2",
      "org.infinispan.stream.impl.TerminalFunctions$CollectIntFunction",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceResolver",
      "org.apache.commons.collections4.map.AbstractHashedMap",
      "org.dom4j.tree.AbstractText",
      "org.infinispan.stream.impl.TerminalFunctions$MinLongFunction",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceSerializer",
      "org.xwiki.component.util.DefaultParameterizedType",
      "org.infinispan.stream.impl.TerminalFunctions$MaxIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$CountFunction",
      "org.dom4j.tree.NamespaceCache",
      "com.xpn.xwiki.objects.ElementInterface",
      "org.infinispan.stream.impl.TerminalFunctions$AverageIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchLongFunction",
      "org.jfree.ui.FilesystemFilter",
      "org.dom4j.tree.FlyweightComment",
      "org.xwiki.model.internal.reference.ExplicitStringDocumentReferenceResolver",
      "org.infinispan.stream.StreamMarshalling$EntryToValueFunction",
      "org.xwiki.model.internal.reference.PathStringDocumentReferenceResolver",
      "org.infinispan.stream.impl.DistributedCacheStream$IteratorSupplier",
      "com.xpn.xwiki.plugin.query.XWikiQuery",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasIfPresentReturnPrevOrNull",
      "org.infinispan.stream.impl.TerminalFunctions$CountDoubleFunction",
      "org.dom4j.tree.QNameCache",
      "org.dom4j.dom.DOMProcessingInstruction",
      "com.xpn.xwiki.doc.merge.MergeConfiguration",
      "org.xwiki.model.internal.reference.AbstractReferenceEntityReferenceResolver",
      "org.infinispan.stream.impl.TerminalFunctions$CollectFunction",
      "org.xwiki.rendering.block.Block",
      "com.xpn.xwiki.objects.classes.PropertyClassInterface",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachDoubleFunction",
      "com.xpn.xwiki.objects.StringProperty",
      "com.xpn.xwiki.objects.ObjectDiff",
      "com.xpn.xwiki.api.Api",
      "org.dom4j.dom.DOMDocumentFactory",
      "com.xpn.xwiki.objects.NumberProperty",
      "org.dom4j.bean.BeanElement",
      "org.dom4j.NodeFilter",
      "org.dom4j.tree.DefaultNamespace",
      "org.dom4j.tree.DefaultComment",
      "org.xwiki.model.internal.reference.DeprecatedDefaultStringEntityReferenceResolver",
      "com.xpn.xwiki.objects.BaseStringProperty",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasIfAbsentReturnBoolean",
      "org.xwiki.model.reference.EntityReferenceResolver",
      "info.informatica.io.FilesystemInfo$TokenizedPath",
      "org.dom4j.tree.AbstractProcessingInstruction",
      "org.xwiki.localization.ContextualLocalizationManager",
      "org.xwiki.model.internal.reference.DeprecatedDefaultReferenceDocumentReferenceResolver",
      "org.xwiki.observation.event.Event",
      "org.dom4j.QName",
      "org.apache.commons.collections4.map.AbstractHashedMap$HashEntry",
      "org.xwiki.model.reference.WikiReference",
      "com.xpn.xwiki.objects.classes.BaseClass",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyDoubleFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueReturnPrevOrNull",
      "org.xwiki.rendering.parser.ParseException",
      "org.xwiki.model.internal.reference.DefaultStringDocumentReferenceResolver",
      "org.infinispan.stream.impl.TerminalFunctions$CountIntFunction",
      "org.xwiki.model.internal.reference.DefaultReferenceDocumentReferenceResolver",
      "org.infinispan.CacheStream",
      "org.dom4j.tree.DefaultElement",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceIntFunction",
      "org.xwiki.bridge.DocumentModelBridge",
      "org.dom4j.Node",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceIntFunction",
      "org.dom4j.util.SingletonStrategy",
      "com.xpn.xwiki.objects.StringListProperty",
      "org.infinispan.commons.marshall.MarshallableFunctions",
      "org.dom4j.tree.DefaultCDATA",
      "org.xwiki.model.reference.SpaceReference",
      "org.dom4j.tree.BackedList",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceDoubleFunction",
      "com.xpn.xwiki.objects.classes.ClassInterface",
      "org.infinispan.commons.marshall.MarshallableFunctions$ReturnReadWriteView",
      "com.xpn.xwiki.objects.ObjectInterface",
      "org.infinispan.commons.marshall.MarshallableFunctions$RemoveIfValueEqualsReturnBoolean",
      "org.xwiki.model.internal.reference.DeprecatedDefaultReferenceDocumentReferenceResolver2",
      "org.xwiki.component.annotation.Role",
      "org.dom4j.tree.AbstractAttribute",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetas",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayGeneratorFunction",
      "org.apache.commons.collections4.OrderedMap",
      "org.dom4j.dom.DOMElement",
      "org.xwiki.model.internal.reference.LocalizedStringEntityReferenceSerializer",
      "org.infinispan.util.SerializableFunction",
      "org.dom4j.util.UserDataElement",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchIntFunction",
      "com.xpn.xwiki.objects.BaseObjectReference",
      "org.dom4j.tree.AbstractEntity",
      "org.infinispan.stream.impl.AbstractCacheStream$IntermediateType",
      "org.apache.commons.collections4.OrderedMapIterator",
      "com.xpn.xwiki.objects.DoubleProperty",
      "org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver",
      "org.dom4j.util.IndexedElement",
      "org.dom4j.util.NonLazyElement",
      "org.dom4j.dom.DOMAttribute",
      "org.dom4j.tree.FlyweightEntity",
      "org.apache.commons.collections4.IterableMap",
      "org.infinispan.distexec.mapreduce.MapReduceManagerImpl$MapCombineTask",
      "org.xwiki.logging.Message",
      "com.xpn.xwiki.api.Element",
      "com.xpn.xwiki.internal.xml.DOMXMLWriter",
      "org.apache.commons.collections4.IterableGet",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueIfAbsentReturnPrevOrNull",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfAbsentReturnPrevOrNull",
      "org.xwiki.rendering.block.AbstractBlock",
      "org.dom4j.bean.BeanAttribute",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachIntFunction",
      "org.xwiki.query.QueryException",
      "org.xwiki.model.internal.reference.DeprecatedExplicitReferenceEntityReferenceResolver",
      "org.xwiki.component.annotation.Component",
      "org.dom4j.tree.DefaultAttribute",
      "org.dom4j.io.XMLWriter",
      "com.xpn.xwiki.doc.XWikiDocumentArchive",
      "org.infinispan.stream.impl.DistributedCacheStream$SegmentListenerNotifier",
      "org.xwiki.model.internal.reference.ExplicitReferenceEntityReferenceResolver",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchLongFunction",
      "org.xwiki.model.reference.DocumentReferenceResolver",
      "org.xwiki.logging.LogLevel",
      "org.infinispan.stream.impl.AbstractCacheStream$CollectionDecomposerConsumer",
      "org.dom4j.rule.Pattern",
      "org.infinispan.commons.marshall.MarshallableFunctions$LambdaWithMetas",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachLongFunction",
      "org.dom4j.dom.DOMEntityReference",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasReturnPrevOrNull",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayLongFunction",
      "org.xwiki.logging.Logger",
      "org.xwiki.model.internal.reference.DeprecatedExplicitReferenceDocumentReferenceResolver2",
      "org.infinispan.commons.marshall.MarshallableFunctions$ReturnReadWriteGet",
      "org.dom4j.tree.AbstractComment",
      "org.infinispan.stream.StreamMarshalling$NonNullPredicate",
      "org.dom4j.Branch",
      "info.informatica.io.FilePatternSpec",
      "com.xpn.xwiki.objects.BaseElement",
      "com.xpn.xwiki.objects.DateProperty",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceLongFunction",
      "org.xwiki.model.reference.ObjectReference",
      "org.dom4j.XPath",
      "org.dom4j.VisitorSupport",
      "com.xpn.xwiki.objects.IntegerProperty",
      "org.xwiki.logging.LogQueue",
      "org.xwiki.model.internal.reference.AbstractEntityReferenceResolver",
      "org.dom4j.tree.FlyweightCDATA",
      "org.dom4j.Element",
      "org.apache.commons.collections4.OrderedIterator",
      "info.informatica.io.FilesystemInfo",
      "org.dom4j.dom.DOMCDATA",
      "org.xwiki.model.reference.EntityReference",
      "org.dom4j.InvalidXPathException",
      "org.xwiki.model.internal.reference.DeprecatedExplicitStringDocumentReferenceResolver",
      "org.dom4j.tree.DefaultText",
      "org.infinispan.stream.StreamMarshalling$AlwaysTruePredicate",
      "com.xpn.xwiki.objects.BaseCollection",
      "org.infinispan.stream.impl.TerminalFunctions$CountLongFunction",
      "com.xpn.xwiki.internal.merge.MergeUtils",
      "org.xwiki.model.EntityType",
      "org.xwiki.model.reference.InvalidEntityReferenceException",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayIntFunction",
      "org.xwiki.rendering.block.MetaDataBlock",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$MinIntFunction",
      "com.xpn.xwiki.objects.FloatProperty",
      "com.xpn.xwiki.objects.LargeStringProperty",
      "org.infinispan.stream.impl.TerminalFunctions$MinFunction",
      "org.infinispan.stream.impl.TerminalFunctions$SummaryStatisticsIntFunction",
      "org.dom4j.tree.AbstractCDATA",
      "com.xpn.xwiki.objects.classes.PropertyClass",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyIntFunction",
      "org.dom4j.bean.BeanMetaData",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueReturnView",
      "org.infinispan.stream.impl.TerminalFunctions$CollectorFunction",
      "org.dom4j.tree.AbstractDocument",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyLongFunction",
      "com.xpn.xwiki.objects.PropertyInterface",
      "org.apache.pdfbox.encoding.SingleByteCharset$Encoder",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueIfPresentReturnPrevOrNull",
      "org.dom4j.Attribute",
      "org.apache.commons.collections4.MapIterator",
      "org.dom4j.Document",
      "org.xwiki.stability.Unstable",
      "org.dom4j.tree.ConcurrentReaderHashMap",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValue",
      "org.infinispan.stream.impl.TerminalFunctions$AverageLongFunction",
      "org.dom4j.tree.ConcurrentReaderHashMap$Entry",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueIfAbsentReturnBoolean",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceFunction",
      "org.dom4j.dom.DOMText",
      "org.xwiki.component.manager.ComponentLookupException",
      "org.dom4j.tree.AbstractDocumentType",
      "com.xpn.xwiki.objects.LongProperty",
      "org.infinispan.stream.impl.TerminalFunctions$SumDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchFunction",
      "org.dom4j.tree.FlyweightAttribute",
      "org.dom4j.CDATA",
      "org.dom4j.IllegalAddException",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfPresentReturnBoolean",
      "org.dom4j.tree.FlyweightText",
      "org.dom4j.tree.AbstractNode",
      "org.dom4j.Visitor",
      "org.xwiki.context.Execution",
      "org.dom4j.tree.ConcurrentReaderHashMap$BarrierLock",
      "com.xpn.xwiki.objects.ListProperty",
      "org.dom4j.CharacterData",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfPresentReturnPrevOrNull",
      "com.xpn.xwiki.objects.DBStringListProperty",
      "com.xpn.xwiki.objects.BaseProperty",
      "org.infinispan.iteration.impl.DistributedEntryRetriever$MapAction",
      "com.xpn.xwiki.doc.merge.MergeResult",
      "org.infinispan.distexec.mapreduce.MapReduceManagerImpl$DataContainerTask",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$CollectLongFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasIfPresentReturnBoolean",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueIfPresentReturnBoolean",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasReturnView",
      "org.infinispan.util.CloseableSupplier",
      "info.informatica.io.WildcardFilter",
      "org.xwiki.model.reference.DocumentReference",
      "org.xwiki.model.internal.reference.DeprecatedRelativeStringEntityReferenceResolver",
      "com.xpn.xwiki.XWikiContext",
      "org.infinispan.stream.impl.TerminalFunctions$CollectDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchLongFunction",
      "com.xpn.xwiki.doc.XWikiDocument",
      "org.infinispan.filter.CacheFilters$ConverterAsCacheEntryFunction",
      "org.xwiki.diff.MergeException",
      "org.xwiki.model.internal.reference.DeprecatedExplicitReferenceEntityReferenceResolver2",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceSerializer",
      "org.apache.commons.collections4.map.LRUMap",
      "org.infinispan.stream.impl.TerminalFunctions$MinDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayFunction",
      "org.xwiki.model.internal.reference.DeprecatedExplicitStringEntityReferenceResolver",
      "org.infinispan.stream.impl.TerminalFunctions$SummaryStatisticsLongFunction",
      "org.apache.commons.collections4.Get",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchFunction",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceCombinerFunction",
      "org.xwiki.rendering.syntax.Syntax",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceLongFunction",
      "org.dom4j.dom.DOMDocumentType",
      "com.xpn.xwiki.web.Utils",
      "org.dom4j.DocumentType",
      "org.xwiki.diff.DiffException",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceResolver",
      "org.infinispan.persistence.spi.AdvancedCacheLoader$CacheLoaderTask",
      "org.infinispan.stream.StreamMarshalling$EntryToKeyFunction",
      "org.dom4j.tree.DefaultDocument",
      "org.xwiki.model.reference.EntityReferenceSerializer",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasIfAbsentReturnPrevOrNull",
      "org.infinispan.commons.marshall.MarshallableFunctions$ReturnReadWriteFind",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachFunction",
      "org.infinispan.stream.impl.TerminalFunctions$MaxFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueReturnView",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceFunction",
      "org.infinispan.stream.impl.TerminalFunctions$SumLongFunction",
      "org.xwiki.model.reference.ObjectPropertyReference",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchIntFunction",
      "org.xwiki.component.manager.ComponentManager",
      "org.dom4j.util.SimpleSingleton",
      "org.xwiki.model.internal.reference.DefaultReferenceEntityReferenceResolver",
      "org.dom4j.dom.DOMDocument",
      "org.infinispan.commons.marshall.MarshallableFunctions$RemoveReturnBoolean",
      "org.xwiki.model.internal.reference.DeprecatedDefaultStringDocumentReferenceResolver",
      "com.xpn.xwiki.internal.xml.XMLWriter",
      "org.dom4j.bean.BeanAttributeList",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchDoubleFunction",
      "org.dom4j.io.OutputFormat",
      "org.dom4j.dom.DOMNamespace",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyFunction",
      "com.xpn.xwiki.api.Collection",
      "org.dom4j.tree.AbstractElement",
      "org.dom4j.DocumentFactory",
      "org.infinispan.filter.CacheFilters$KeyValueFilterAsPredicate",
      "org.dom4j.DocumentException",
      "org.infinispan.stream.impl.DistributedCacheStream$HandOffConsumer",
      "org.dom4j.tree.BaseElement",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceDoubleFunction",
      "org.dom4j.Comment",
      "org.infinispan.stream.impl.TerminalFunctions$SummaryStatisticsDoubleFunction",
      "org.dom4j.tree.DefaultEntity",
      "org.xwiki.model.internal.reference.RelativeStringEntityReferenceResolver",
      "org.xwiki.model.reference.LocalDocumentReference",
      "org.infinispan.stream.impl.AbstractCacheStream",
      "info.informatica.io.FilePatternSpec$FilePattern",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValue",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfAbsentReturnBoolean",
      "org.xwiki.rendering.block.match.BlockMatcher",
      "org.xwiki.model.reference.EntityReferenceProvider",
      "com.xpn.xwiki.objects.BaseObject",
      "org.dom4j.tree.DefaultProcessingInstruction",
      "com.xpn.xwiki.api.Object",
      "org.infinispan.stream.impl.TerminalFunctions$MaxDoubleFunction",
      "org.dom4j.Text",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchFunction",
      "org.dom4j.util.UserDataAttribute",
      "org.dom4j.tree.AbstractCharacterData",
      "org.xwiki.model.internal.reference.DeprecatedDefaultReferenceEntityReferenceResolver",
      "org.apache.commons.collections4.BoundedMap",
      "org.xwiki.rendering.block.CompatibilityBlock",
      "com.xpn.xwiki.objects.CompatibilityObjectInterface",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfEqualsReturnBoolean",
      "org.infinispan.filter.CacheFilters$FilterConverterAsCacheEntryFunction",
      "org.apache.commons.collections4.Put",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchDoubleFunction",
      "org.xwiki.model.internal.reference.DeprecatedExplicitReferenceDocumentReferenceResolver",
      "org.dom4j.dom.DOMComment",
      "org.xwiki.diff.DiffManager",
      "org.infinispan.stream.impl.TerminalFunctions$AverageDoubleFunction",
      "com.xpn.xwiki.plugin.query.XWikiCriteria",
      "org.xwiki.logging.event.LogEvent",
      "org.apache.commons.collections4.map.AbstractLinkedMap$LinkEntry"
    );
  } 
}
