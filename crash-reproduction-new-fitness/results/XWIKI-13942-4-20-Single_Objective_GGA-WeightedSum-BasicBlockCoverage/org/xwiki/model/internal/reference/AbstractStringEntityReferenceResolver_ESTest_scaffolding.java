/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sun May 17 17:15:34 UTC 2020
 */

package org.xwiki.model.internal.reference;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

@EvoSuiteClassExclude
public class AbstractStringEntityReferenceResolver_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.model.internal.reference.AbstractStringEntityReferenceResolver"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(AbstractStringEntityReferenceResolver_ESTest_scaffolding.class.getClassLoader() ,
      "org.xwiki.component.phase.Initializable",
      "org.aspectj.lang.reflect.AjType",
      "org.xwiki.model.reference.EntityReference",
      "org.infinispan.commons.marshall.MarshallableFunctions$Remove",
      "org.infinispan.stream.impl.TerminalFunctions$MaxLongFunction",
      "org.xwiki.component.phase.InitializationException",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueReturnPrevOrNull",
      "org.apache.commons.lang3.StringUtils",
      "org.infinispan.stream.impl.TerminalFunctions$CountLongFunction",
      "org.infinispan.iteration.impl.LocalEntryRetriever$MapAction",
      "org.infinispan.commons.marshall.MarshallableFunctions$RemoveReturnPrevOrNull",
      "org.xwiki.model.EntityType",
      "com.google.inject.internal.MoreTypes$GenericArrayTypeImpl",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$SumIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$MinIntFunction",
      "org.xwiki.model.internal.reference.SymbolScheme",
      "org.infinispan.stream.impl.TerminalFunctions$CollectIntFunction",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceResolver",
      "com.google.inject.internal.MoreTypes$WildcardTypeImpl",
      "org.infinispan.stream.impl.TerminalFunctions$MinFunction",
      "org.infinispan.stream.impl.TerminalFunctions$SummaryStatisticsIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$MinLongFunction",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceSerializer",
      "org.xwiki.component.util.DefaultParameterizedType",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueReturnView",
      "org.infinispan.stream.impl.TerminalFunctions$CollectorFunction",
      "org.infinispan.stream.impl.TerminalFunctions$MaxIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$CountFunction",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyLongFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueIfPresentReturnPrevOrNull",
      "com.google.gson.internal.$Gson$Types$GenericArrayTypeImpl",
      "org.infinispan.stream.impl.TerminalFunctions$AverageIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchLongFunction",
      "org.xwiki.stability.Unstable",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValue",
      "org.infinispan.stream.impl.TerminalFunctions$AverageLongFunction",
      "org.infinispan.stream.StreamMarshalling$EntryToValueFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueIfAbsentReturnBoolean",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasIfPresentReturnPrevOrNull",
      "org.infinispan.stream.impl.TerminalFunctions$CountDoubleFunction",
      "com.google.inject.internal.MoreTypes",
      "org.infinispan.stream.impl.TerminalFunctions$CollectFunction",
      "com.github.benmanes.caffeine.cache.LocalAsyncLoadingCache$AsyncBulkCompleter",
      "org.infinispan.stream.impl.TerminalFunctions$SumDoubleFunction",
      "com.google.common.reflect.Types",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfPresentReturnBoolean",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachDoubleFunction",
      "com.fasterxml.jackson.databind.JavaType",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfPresentReturnPrevOrNull",
      "org.infinispan.iteration.impl.DistributedEntryRetriever$MapAction",
      "com.google.common.reflect.Types$GenericArrayTypeImpl",
      "org.infinispan.distexec.mapreduce.MapReduceManagerImpl$DataContainerTask",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$CollectLongFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasIfPresentReturnBoolean",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueIfPresentReturnBoolean",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasIfAbsentReturnBoolean",
      "org.xwiki.model.reference.EntityReferenceResolver",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasReturnView",
      "com.google.inject.internal.MoreTypes$ParameterizedTypeImpl",
      "org.xwiki.model.reference.DocumentReference",
      "org.infinispan.stream.impl.TerminalFunctions$CollectDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchLongFunction",
      "com.google.common.reflect.Types$ParameterizedTypeImpl",
      "org.infinispan.filter.CacheFilters$ConverterAsCacheEntryFunction",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyDoubleFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueReturnPrevOrNull",
      "org.infinispan.stream.impl.TerminalFunctions$CountIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceIntFunction",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceSerializer",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceIntFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions",
      "org.infinispan.stream.impl.TerminalFunctions$MinDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayFunction",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceDoubleFunction",
      "org.xwiki.model.internal.reference.DefaultSymbolScheme",
      "org.infinispan.commons.marshall.MarshallableFunctions$ReturnReadWriteView",
      "org.infinispan.stream.impl.TerminalFunctions$SummaryStatisticsLongFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$RemoveIfValueEqualsReturnBoolean",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchFunction",
      "org.xwiki.component.annotation.Role",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceCombinerFunction",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceLongFunction",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceResolver",
      "org.infinispan.persistence.spi.AdvancedCacheLoader$CacheLoaderTask",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetas",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayGeneratorFunction",
      "org.infinispan.stream.StreamMarshalling$EntryToKeyFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasIfAbsentReturnPrevOrNull",
      "org.xwiki.model.reference.EntityReferenceSerializer",
      "org.infinispan.commons.marshall.MarshallableFunctions$ReturnReadWriteFind",
      "org.xwiki.model.internal.reference.LocalizedStringEntityReferenceSerializer",
      "org.infinispan.util.SerializableFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachFunction",
      "com.google.gson.internal.$Gson$Types$WildcardTypeImpl",
      "org.infinispan.stream.impl.TerminalFunctions$MaxFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueReturnView",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceFunction",
      "org.infinispan.stream.impl.TerminalFunctions$SumLongFunction",
      "org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchIntFunction",
      "com.google.common.reflect.Types$WildcardTypeImpl",
      "org.infinispan.distexec.mapreduce.MapReduceManagerImpl$MapCombineTask",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueIfAbsentReturnPrevOrNull",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfAbsentReturnPrevOrNull",
      "org.infinispan.commons.marshall.MarshallableFunctions$RemoveReturnBoolean",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachIntFunction",
      "org.xwiki.component.annotation.Component",
      "org.xwiki.model.internal.reference.DefaultSymbolScheme$1",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchDoubleFunction",
      "org.xwiki.text.StringUtils",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyFunction",
      "com.fasterxml.jackson.core.type.ResolvedType",
      "com.google.inject.internal.MoreTypes$CompositeType",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchLongFunction",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceDoubleFunction",
      "com.fasterxml.jackson.databind.type.TypeBindings",
      "org.infinispan.commons.marshall.MarshallableFunctions$LambdaWithMetas",
      "org.infinispan.stream.impl.TerminalFunctions$SummaryStatisticsDoubleFunction",
      "org.xwiki.model.internal.reference.RelativeStringEntityReferenceResolver",
      "org.xwiki.model.reference.LocalDocumentReference",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachLongFunction",
      "com.google.common.reflect.Types$NativeTypeVariableEquals",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasReturnPrevOrNull",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValue",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfAbsentReturnBoolean",
      "org.xwiki.model.reference.EntityReferenceProvider",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayLongFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$ReturnReadWriteGet",
      "org.infinispan.stream.impl.TerminalFunctions$MaxDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchFunction",
      "com.google.common.reflect.Types$JavaVersion",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfEqualsReturnBoolean",
      "org.infinispan.filter.CacheFilters$FilterConverterAsCacheEntryFunction",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceLongFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchDoubleFunction",
      "com.google.gson.internal.$Gson$Types$ParameterizedTypeImpl",
      "org.infinispan.stream.impl.TerminalFunctions$AverageDoubleFunction",
      "org.xwiki.model.internal.reference.AbstractEntityReferenceResolver"
    );
  } 
}
