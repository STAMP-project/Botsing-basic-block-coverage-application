/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Thu May 14 12:45:51 UTC 2020
 */

package org.xwiki.query.internal;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class AbstractQueryFilter_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.query.internal.AbstractQueryFilter"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(AbstractQueryFilter_ESTest_scaffolding.class.getClassLoader() ,
      "org.xwiki.query.QueryFilter",
      "org.infinispan.commons.marshall.MarshallableFunctions$Remove",
      "org.infinispan.stream.impl.TerminalFunctions$MaxLongFunction",
      "org.infinispan.stream.StreamMarshalling$EqualityPredicate",
      "org.infinispan.stream.StreamMarshalling$AlwaysTruePredicate",
      "org.infinispan.stream.impl.TerminalFunctions$CountLongFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$RemoveReturnPrevOrNull",
      "org.apache.lucene.search.QueryCache",
      "org.infinispan.stream.impl.DistributedCacheStream",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$SumIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$MinIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$CollectIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$MinFunction",
      "org.infinispan.stream.impl.TerminalFunctions$SummaryStatisticsIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$MinLongFunction",
      "org.infinispan.stream.impl.TerminalFunctions$CollectorFunction",
      "org.infinispan.stream.impl.TerminalFunctions$MaxIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$CountFunction",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyLongFunction",
      "org.apache.lucene.search.LRUQueryCache$LeafCache",
      "org.apache.lucene.util.Accountable",
      "org.apache.lucene.search.LRUQueryCache$MinSegmentSizePredicate",
      "org.infinispan.stream.impl.TerminalFunctions$AverageIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchLongFunction",
      "org.jfree.ui.FilesystemFilter",
      "org.xwiki.query.internal.UniqueDocumentFilter",
      "org.infinispan.stream.impl.TerminalFunctions$AverageLongFunction",
      "org.infinispan.stream.StreamMarshalling$EntryToValueFunction",
      "org.infinispan.stream.impl.DistributedCacheStream$IteratorSupplier",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceFunction",
      "org.infinispan.stream.impl.TerminalFunctions$CountDoubleFunction",
      "org.apache.lucene.search.LRUQueryCache",
      "org.infinispan.stream.impl.TerminalFunctions$CollectFunction",
      "org.infinispan.stream.impl.TerminalFunctions$SumDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchFunction",
      "org.xwiki.component.annotation.InstantiationStrategy",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$CollectLongFunction",
      "info.informatica.io.FilesystemInfo$TokenizedPath",
      "org.infinispan.util.CloseableSupplier",
      "info.informatica.io.WildcardFilter",
      "org.xwiki.component.descriptor.ComponentInstantiationStrategy",
      "org.infinispan.stream.impl.TerminalFunctions$CollectDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchLongFunction",
      "org.infinispan.filter.CacheFilters$ConverterAsCacheEntryFunction",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$CountIntFunction",
      "org.infinispan.CacheStream",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceIntFunction",
      "org.apache.commons.collections.ListUtils",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$MinDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayFunction",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceDoubleFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$ReturnReadWriteView",
      "org.infinispan.stream.impl.TerminalFunctions$SummaryStatisticsLongFunction",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchFunction",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceCombinerFunction",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceLongFunction",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayGeneratorFunction",
      "org.infinispan.stream.StreamMarshalling$EntryToKeyFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$ReturnReadWriteFind",
      "org.infinispan.util.SerializableFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachFunction",
      "org.infinispan.stream.impl.TerminalFunctions$MaxFunction",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceFunction",
      "org.infinispan.stream.impl.AbstractCacheStream$IntermediateType",
      "org.infinispan.stream.impl.TerminalFunctions$SumLongFunction",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchIntFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$RemoveReturnBoolean",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachIntFunction",
      "org.xwiki.query.internal.CountDocumentFilter",
      "org.xwiki.component.annotation.Component",
      "org.apache.lucene.search.Weight",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchDoubleFunction",
      "org.infinispan.stream.impl.DistributedCacheStream$SegmentListenerNotifier",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyFunction",
      "org.infinispan.filter.CacheFilters$KeyValueFilterAsPredicate",
      "org.xwiki.query.internal.AbstractQueryFilter",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchLongFunction",
      "org.infinispan.stream.impl.AbstractCacheStream$CollectionDecomposerConsumer",
      "org.infinispan.stream.impl.DistributedCacheStream$HandOffConsumer",
      "org.apache.lucene.search.LRUQueryCache$CachingWrapperWeight",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$SummaryStatisticsDoubleFunction",
      "org.infinispan.stream.impl.AbstractCacheStream",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachLongFunction",
      "info.informatica.io.FilePatternSpec$FilePattern",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayLongFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$ReturnReadWriteGet",
      "org.infinispan.stream.StreamMarshalling$NonNullPredicate",
      "org.infinispan.stream.impl.TerminalFunctions$MaxDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchFunction",
      "info.informatica.io.FilePatternSpec",
      "org.infinispan.filter.CacheFilters$FilterConverterAsCacheEntryFunction",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceLongFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AverageDoubleFunction",
      "org.apache.lucene.search.ConstantScoreWeight",
      "info.informatica.io.FilesystemInfo"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.slf4j.Logger", false, AbstractQueryFilter_ESTest_scaffolding.class.getClassLoader()));
  }
}
