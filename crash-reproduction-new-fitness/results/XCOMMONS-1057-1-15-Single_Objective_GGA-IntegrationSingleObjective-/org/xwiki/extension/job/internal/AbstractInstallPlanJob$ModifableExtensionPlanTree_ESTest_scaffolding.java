/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Sun May 17 01:01:48 UTC 2020
 */

package org.xwiki.extension.job.internal;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.xwiki.extension.job.internal.AbstractInstallPlanJob$ModifableExtensionPlanTree"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest_scaffolding.class.getClassLoader() ,
      "org.jfree.data.time.TimeTableXYDataset",
      "org.apache.hadoop.util.Shell$OSType",
      "org.apache.hadoop.fs.FileSystem",
      "org.infinispan.stream.impl.TerminalFunctions$MaxLongFunction",
      "org.infinispan.stream.StreamMarshalling$EqualityPredicate",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueReturnPrevOrNull",
      "org.jfree.data.time.Minute",
      "org.xwiki.extension.version.Version",
      "org.xwiki.extension.version.VersionRange",
      "org.jfree.data.general.ValueDataset",
      "org.infinispan.commons.marshall.MarshallableFunctions$RemoveReturnPrevOrNull",
      "org.xwiki.job.AbstractJob",
      "org.apache.hadoop.conf.Configuration",
      "org.xwiki.observation.event.FilterableEvent",
      "org.xwiki.extension.wrap.AbstractWrappingObject",
      "org.xwiki.component.manager.CompatibilityComponentManager",
      "org.jfree.data.general.AbstractSeriesDataset",
      "org.jfree.data.xy.DefaultTableXYDataset",
      "org.infinispan.stream.impl.DistributedCacheStream",
      "org.apache.hadoop.classification.InterfaceStability$Stable",
      "org.jfree.data.RangeInfo",
      "org.infinispan.stream.impl.TerminalFunctions$SumIntFunction",
      "org.jfree.data.general.CombinedDataset",
      "org.xwiki.extension.repository.AbstractExtensionRepository",
      "com.google.inject.internal.MoreTypes$WildcardTypeImpl",
      "org.jfree.data.jdbc.JDBCXYDataset",
      "org.apache.commons.collections.IterableMap",
      "org.xwiki.component.util.DefaultParameterizedType",
      "org.infinispan.stream.impl.TerminalFunctions$MaxIntFunction",
      "org.apache.commons.vfs2.impl.DefaultVfsComponentContext",
      "org.xwiki.component.descriptor.DefaultComponentRole",
      "org.jfree.data.statistics.HistogramType",
      "org.xwiki.observation.event.DocumentUpdateEvent",
      "org.jfree.data.xy.DefaultXYDataset",
      "org.xwiki.observation.EventListener",
      "org.jfree.data.contour.ContourDataset",
      "org.infinispan.stream.impl.TerminalFunctions$AverageIntFunction",
      "org.jfree.data.time.TimeSeriesCollection",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasIfPresentReturnPrevOrNull",
      "org.jfree.data.xy.DefaultHighLowDataset",
      "org.xwiki.observation.event.DocumentSaveEvent",
      "org.infinispan.stream.impl.TerminalFunctions$CountDoubleFunction",
      "org.jfree.data.time.TimePeriodFormatException",
      "org.xwiki.component.manager.ComponentRepositoryException",
      "com.mchange.v2.c3p0.C3P0ProxyConnection",
      "org.apache.commons.dbcp2.PoolingConnection$StatementType",
      "org.xwiki.observation.internal.DefaultObservationManager",
      "org.xwiki.extension.job.internal.AbstractInstallPlanJob",
      "org.apache.commons.vfs2.provider.VfsComponentContext",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachDoubleFunction",
      "org.apache.commons.vfs2.FileSystemOptions",
      "org.codehaus.classworlds.BytesURLStreamHandler",
      "com.google.common.collect.MapMakerInternalMap$Strength",
      "com.google.common.base.Equivalence$Equals",
      "org.jfree.data.time.Second",
      "org.jfree.data.xy.YIntervalDataItem",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasIfAbsentReturnBoolean",
      "org.jfree.data.xy.XIntervalSeries",
      "org.xwiki.observation.event.ActionExecutionEvent",
      "org.xwiki.extension.ExtensionDependency",
      "org.apache.tika.fork.MemoryURLStreamHandler",
      "org.jfree.data.KeyedValues",
      "org.jfree.data.xy.XYIntervalSeries",
      "com.google.common.reflect.Types$ParameterizedTypeImpl",
      "org.xwiki.extension.job.plan.ExtensionPlanAction$Action",
      "org.xwiki.extension.ExtensionLicense",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueReturnPrevOrNull",
      "org.infinispan.stream.impl.TerminalFunctions$CountIntFunction",
      "org.infinispan.CacheStream",
      "com.google.common.collect.MapMakerInternalMap$ValueReference",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceIntFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions",
      "org.apache.commons.dbcp2.DelegatingPreparedStatement",
      "org.infinispan.commons.marshall.MarshallableFunctions$RemoveIfValueEqualsReturnBoolean",
      "org.xwiki.extension.wrap.WrappingRatingExtension",
      "org.jfree.data.xy.XYZDataset",
      "org.xwiki.component.annotation.Role",
      "org.apache.commons.dbcp2.PoolableConnection",
      "org.apache.commons.vfs2.FileSystemManager",
      "org.jfree.data.general.DatasetChangeListener",
      "org.jfree.data.statistics.MultiValueCategoryDataset",
      "org.apache.hadoop.util.Shell$1",
      "org.xwiki.extension.rating.Rating",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayGeneratorFunction",
      "org.jfree.data.gantt.TaskSeries",
      "org.jfree.data.xy.YIntervalSeries",
      "org.xwiki.observation.event.BeginFoldEvent",
      "org.python.core.PySystemStateTest",
      "org.xwiki.extension.test.FileExtension",
      "org.apache.hadoop.util.Shell$ExitCodeException",
      "org.infinispan.util.SerializableFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchIntFunction",
      "org.jfree.data.statistics.SimpleHistogramBin",
      "org.apache.hadoop.conf.Configuration$DeprecationContext",
      "org.xwiki.extension.job.internal.AbstractInstallPlanJob$ModifableExtensionPlanNode",
      "com.google.common.collect.Interner",
      "org.jfree.data.time.Millisecond",
      "com.google.common.collect.Interners$1",
      "org.apache.commons.vfs2.FileName",
      "org.jfree.data.general.DefaultKeyedValueDataset",
      "org.apache.commons.dbcp2.AbandonedTrace",
      "org.jfree.data.general.SeriesChangeEvent",
      "org.xwiki.observation.event.filter.EventFilter",
      "org.xwiki.extension.test.EmptyExtension",
      "org.infinispan.distexec.mapreduce.MapReduceManagerImpl$MapCombineTask",
      "org.xwiki.extension.wrap.WrappingCoreExtension",
      "org.xwiki.extension.CoreExtensionFile",
      "org.apache.commons.configuration.VFSFileSystem$VFSURLStreamHandler",
      "org.xwiki.observation.event.ApplicationStoppedEvent",
      "com.google.common.base.Equivalence$Identity",
      "org.infinispan.stream.impl.DistributedCacheStream$SegmentListenerNotifier",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchLongFunction",
      "org.infinispan.stream.impl.AbstractCacheStream$CollectionDecomposerConsumer",
      "org.xwiki.observation.AbstractThreadEventListener",
      "org.apache.hadoop.util.StringUtils",
      "org.jfree.data.xy.DefaultWindDataset",
      "org.xwiki.observation.event.CancelableEvent",
      "org.xwiki.extension.job.plan.internal.DefaultExtensionPlanAction",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachLongFunction",
      "org.jfree.data.xy.IntervalXYZDataset",
      "org.apache.commons.vfs2.impl.URLStreamHandlerProxy",
      "org.xwiki.observation.event.AbstractCancelableEvent",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayLongFunction",
      "org.jfree.data.time.FixedMillisecond",
      "org.apache.tika.fork.MemoryURLStreamHandlerFactory",
      "org.jfree.data.Value",
      "org.jfree.data.time.TimePeriodAnchor",
      "org.apache.commons.dbcp2.PoolingDriver$PoolGuardConnectionWrapper",
      "org.jfree.data.gantt.Task",
      "org.jfree.data.general.DefaultValueDataset",
      "com.google.gson.internal.$Gson$Types$ParameterizedTypeImpl",
      "org.xwiki.extension.version.InvalidVersionRangeException",
      "org.jfree.data.contour.NonGridContourDataset",
      "com.google.common.collect.MapMakerInternalMap",
      "info.informatica.io.FilesystemInfo",
      "org.jfree.data.xy.VectorSeriesCollection",
      "org.jfree.date.SerialDate",
      "org.aspectj.lang.reflect.AjType",
      "org.xwiki.extension.rating.ExtensionRating",
      "org.xwiki.extension.version.Version$Type",
      "org.jfree.data.DomainOrder",
      "org.infinispan.stream.StreamMarshalling$AlwaysTruePredicate",
      "org.infinispan.stream.impl.TerminalFunctions$CountLongFunction",
      "org.xwiki.extension.wrap.WrappingInstalledExtension",
      "org.xwiki.extension.test.EmptyLocalExtensionFile",
      "org.xwiki.extension.AbstractExtension",
      "org.apache.hadoop.conf.Configuration$1",
      "org.jfree.data.jdbc.JDBCPieDataset",
      "org.jfree.data.gantt.SlidingGanttCategoryDataset",
      "org.xwiki.component.event.AbstractComponentDescriptorEvent",
      "org.xwiki.component.internal.StackingComponentEventManager$ComponentEventEntry",
      "org.xwiki.component.event.ComponentDescriptorRemovedEvent",
      "org.xwiki.observation.event.AbstractFilterableEvent",
      "com.google.inject.internal.MoreTypes$GenericArrayTypeImpl",
      "org.xwiki.extension.rating.RatingExtension",
      "org.jfree.data.DefaultKeyedValues2D",
      "org.jfree.data.xy.DefaultOHLCDataset",
      "org.apache.hadoop.util.Shell$CommandExecutor",
      "org.infinispan.stream.impl.TerminalFunctions$MinFunction",
      "org.infinispan.stream.impl.TerminalFunctions$SummaryStatisticsIntFunction",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyIntFunction",
      "org.jfree.data.time.Day",
      "org.jfree.data.general.SeriesDataset",
      "org.infinispan.stream.impl.TerminalFunctions$CollectorFunction",
      "com.google.common.collect.MapMaker$NullConcurrentMap",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchIntFunction",
      "org.jfree.data.xy.XYDataset",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyLongFunction",
      "org.xwiki.extension.DefaultExtensionDependency",
      "org.apache.hadoop.fs.ChecksumFileSystem",
      "org.apache.commons.dbcp2.DelegatingConnection",
      "org.xwiki.extension.job.internal.AbstractInstallPlanJob$ModifableExtensionPlanTree",
      "org.xwiki.job.GroupedJob",
      "org.xwiki.stability.Unstable",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValue",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceFunction",
      "org.xwiki.extension.ExtensionId",
      "org.xwiki.component.manager.ComponentLookupException",
      "org.jfree.data.xy.AbstractXYZDataset",
      "org.jfree.data.time.RegularTimePeriod",
      "com.google.inject.internal.MoreTypes",
      "org.xwiki.extension.wrap.WrappingLocalExtension",
      "org.jfree.data.time.TimeSeries",
      "org.xwiki.component.annotation.InstantiationStrategy",
      "org.xwiki.extension.test.ResourceExtension",
      "org.jfree.data.general.CombinationDataset",
      "org.apache.commons.dbcp2.DelegatingStatement",
      "org.jfree.data.xy.IntervalXYDataset",
      "org.jfree.data.xy.Vector",
      "org.infinispan.distexec.mapreduce.MapReduceManagerImpl$DataContainerTask",
      "org.apache.hadoop.conf.Configuration$IntegerRanges",
      "org.jfree.data.general.DefaultPieDataset",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchDoubleFunction",
      "com.google.common.collect.GenericMapMaker",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasIfPresentReturnBoolean",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueIfPresentReturnBoolean",
      "org.apache.hadoop.fs.FsUrlStreamHandler",
      "org.jfree.data.category.DefaultIntervalCategoryDataset",
      "org.apache.commons.pool2.KeyedObjectPool",
      "org.infinispan.util.CloseableSupplier",
      "org.jfree.data.xy.XIntervalSeriesCollection",
      "org.xwiki.component.descriptor.ComponentInstantiationStrategy",
      "org.xwiki.extension.version.internal.DefaultVersion$Element",
      "org.jfree.data.category.CategoryDataset",
      "org.jfree.data.general.DefaultHeatMapDataset",
      "org.jfree.data.ComparableObjectItem",
      "org.jfree.data.xy.XYSeries",
      "org.infinispan.stream.impl.TerminalFunctions$CollectDoubleFunction",
      "org.xwiki.component.descriptor.ComponentRole",
      "org.eclipse.sisu.space.ResourceEnumeration",
      "org.apache.hadoop.conf.Configuration$ParsedTimeDuration",
      "org.infinispan.filter.CacheFilters$ConverterAsCacheEntryFunction",
      "org.osgi.service.url.URLStreamHandlerSetter",
      "org.osgi.service.url.URLStreamHandlerService",
      "org.apache.commons.collections.map.UnmodifiableMap",
      "org.infinispan.stream.impl.TerminalFunctions$MinDoubleFunction",
      "org.xwiki.component.manager.ComponentLifecycleException",
      "org.infinispan.stream.impl.TerminalFunctions$SummaryStatisticsLongFunction",
      "org.jfree.data.gantt.TaskSeriesCollection",
      "org.apache.commons.pool2.TrackedUse",
      "org.xwiki.extension.ExtensionException",
      "org.xwiki.extension.wrap.WrappingExtension",
      "org.xwiki.extension.DefaultExtensionScm",
      "org.apache.commons.collections.map.AbstractMapDecorator",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceCombinerFunction",
      "org.infinispan.persistence.spi.AdvancedCacheLoader$CacheLoaderTask",
      "org.apache.hadoop.fs.Path",
      "org.apache.hadoop.classification.InterfaceAudience$Public",
      "org.apache.hadoop.conf.Configurable",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasIfAbsentReturnPrevOrNull",
      "com.google.common.base.Function",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachFunction",
      "org.jfree.data.xy.MatrixSeriesCollection",
      "com.google.common.collect.MapMakerInternalMap$Strength$2",
      "org.xwiki.extension.repository.ExtensionRepositoryId",
      "org.apache.commons.dbcp2.PStmtKey",
      "com.google.common.collect.MapMakerInternalMap$Strength$1",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceFunction",
      "org.infinispan.stream.impl.TerminalFunctions$SumLongFunction",
      "org.jfree.data.xy.MatrixSeries",
      "com.google.common.collect.MapMakerInternalMap$Strength$3",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchIntFunction",
      "org.xwiki.component.manager.ComponentManager",
      "org.xwiki.extension.AbstractExtensionScmConnection",
      "com.google.common.reflect.Types$WildcardTypeImpl",
      "org.jfree.data.time.TimeSeriesDataItem",
      "org.jfree.date.MonthConstants",
      "com.google.common.collect.Interners$WeakInterner",
      "org.jfree.data.xy.XYBarDataset",
      "org.infinispan.filter.CacheFilters$KeyValueFilterAsPredicate",
      "org.apache.commons.vfs2.provider.FileReplicator",
      "org.jfree.data.xy.VectorXYDataset",
      "org.xwiki.observation.event.EndEvent",
      "org.jfree.data.time.Hour",
      "org.xwiki.extension.job.internal.AbstractExtensionPlanJob",
      "org.jfree.data.xy.XYDatasetTableModel",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$SummaryStatisticsDoubleFunction",
      "org.xwiki.extension.ExtensionScm",
      "org.xwiki.observation.event.DocumentDeleteEvent",
      "org.infinispan.stream.impl.AbstractCacheStream",
      "com.google.common.reflect.Types$NativeTypeVariableEquals",
      "info.informatica.io.FilePatternSpec$FilePattern",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfAbsentReturnBoolean",
      "org.xwiki.extension.AbstractExtensionDependency",
      "com.google.common.base.Equivalence",
      "org.jfree.data.statistics.StatisticalCategoryDataset",
      "com.google.common.collect.Interners",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchFunction",
      "org.jfree.data.Values",
      "org.jfree.data.KeyedValues2D",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$AverageDoubleFunction",
      "org.jfree.data.general.SeriesException",
      "org.apache.commons.collections.Unmodifiable",
      "org.jfree.data.general.CombinedDataset$DatasetInfo",
      "org.xwiki.extension.ExtensionFile",
      "org.infinispan.commons.marshall.MarshallableFunctions$Remove",
      "com.google.common.collect.MapMaker$NullComputingConcurrentMap",
      "org.apache.hadoop.io.Writable",
      "com.google.common.collect.ComputingConcurrentHashMap",
      "org.apache.hadoop.classification.InterfaceAudience$Private",
      "org.apache.commons.vfs2.provider.TemporaryFileStore",
      "org.xwiki.component.event.ComponentDescriptorAddedEvent",
      "org.infinispan.iteration.impl.LocalEntryRetriever$MapAction",
      "org.jfree.data.contour.DefaultContourDataset",
      "org.jfree.data.general.SubSeriesDataset",
      "org.xwiki.extension.job.ExtensionRequest",
      "org.apache.commons.collections.MapIterator",
      "org.jfree.data.xy.AbstractIntervalXYDataset",
      "org.jfree.data.xy.OHLCDataItem",
      "org.infinispan.stream.impl.TerminalFunctions$CollectIntFunction",
      "org.apache.commons.vfs2.FileSystemException",
      "org.jfree.data.xy.YIntervalSeriesCollection",
      "org.jfree.data.xy.AbstractXYDataset",
      "org.infinispan.stream.impl.TerminalFunctions$MinLongFunction",
      "org.jfree.data.general.HeatMapDataset",
      "org.infinispan.stream.impl.TerminalFunctions$CountFunction",
      "com.google.common.base.Predicate",
      "org.xwiki.extension.test.FileExtensionRepository",
      "com.google.gson.internal.$Gson$Types$GenericArrayTypeImpl",
      "org.infinispan.stream.impl.TerminalFunctions$AnyMatchLongFunction",
      "org.jfree.data.xy.DefaultIntervalXYDataset",
      "org.jfree.ui.FilesystemFilter",
      "org.infinispan.stream.StreamMarshalling$EntryToValueFunction",
      "org.infinispan.stream.impl.DistributedCacheStream$IteratorSupplier",
      "org.xwiki.extension.CoreExtension",
      "org.xwiki.observation.event.BeginEvent",
      "org.xwiki.script.wrap.AbstractWrappingObject",
      "org.apache.commons.pool2.ObjectPool",
      "org.infinispan.stream.impl.TerminalFunctions$CollectFunction",
      "org.jfree.data.statistics.BoxAndWhiskerItem",
      "org.jfree.data.category.CategoryToPieDataset",
      "org.apache.commons.pool2.KeyedPooledObjectFactory",
      "com.google.common.reflect.Types",
      "org.jfree.data.DefaultKeyedValues",
      "org.jfree.data.statistics.DefaultBoxAndWhiskerXYDataset",
      "org.apache.commons.dbcp2.PoolableConnectionMXBean",
      "org.jfree.data.statistics.BoxAndWhiskerCategoryDataset",
      "org.jfree.data.time.TimePeriodValues",
      "org.apache.commons.vfs2.FileObject",
      "org.xwiki.extension.test.ResourceExtensionRepository",
      "org.xwiki.extension.version.VersionRangeCollection",
      "com.google.inject.internal.MoreTypes$ParameterizedTypeImpl",
      "org.jfree.data.time.DynamicTimeSeriesCollection$ValueSequence",
      "org.xwiki.component.internal.StackingComponentEventManager",
      "org.xwiki.observation.event.Event",
      "org.jfree.data.xy.CategoryTableXYDataset",
      "com.google.common.base.Preconditions",
      "com.google.common.collect.MapMaker",
      "org.xwiki.extension.version.IncompatibleVersionConstraintException",
      "org.jfree.data.general.DefaultKeyedValues2DDataset",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyDoubleFunction",
      "com.google.common.collect.MapMaker$ComputingMapAdapter",
      "org.jfree.data.time.Quarter",
      "org.jfree.data.time.TimePeriodValue",
      "org.jfree.data.time.DynamicTimeSeriesCollection",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceDoubleFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$ReturnReadWriteView",
      "org.xwiki.extension.DefaultExtensionAuthor",
      "org.jfree.data.gantt.XYTaskDataset",
      "org.jfree.data.DomainInfo",
      "org.xwiki.extension.job.plan.internal.DefaultExtensionPlanTree",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetas",
      "org.apache.commons.vfs2.provider.DefaultURLConnection",
      "org.xwiki.component.event.ComponentDescriptorEvent",
      "org.apache.hadoop.conf.Configuration$NegativeCacheSentinel",
      "org.jfree.data.xy.XYDomainInfo",
      "org.jfree.data.ComparableObjectSeries",
      "org.xwiki.component.descriptor.ComponentDescriptor",
      "org.python.core.PySystemStateTest$TestJBossURLStreamHandler",
      "org.xwiki.extension.AbstractRatingExtension",
      "org.jfree.data.xy.XYDataItem",
      "org.eclipse.sisu.space.ResourceEnumeration$NestedJarHandler",
      "org.jfree.data.general.DatasetGroup",
      "org.jfree.data.DefaultKeyedValue",
      "org.jfree.data.general.KeyedValueDataset",
      "org.xwiki.observation.internal.DefaultObservationManager$RegisteredListener",
      "org.jfree.data.xy.XYIntervalDataItem",
      "org.jfree.data.xy.OHLCDataset",
      "org.infinispan.stream.impl.AbstractCacheStream$IntermediateType",
      "org.jfree.data.xy.VectorDataItem",
      "org.xwiki.extension.ExtensionAuthor",
      "org.jfree.data.time.Month",
      "org.xwiki.observation.event.EndFoldEvent",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueIfAbsentReturnPrevOrNull",
      "org.xwiki.observation.ObservationManager",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfAbsentReturnPrevOrNull",
      "org.infinispan.stream.impl.TerminalFunctions$ForEachIntFunction",
      "org.xwiki.component.annotation.Component",
      "org.jfree.data.general.WaferMapDataset",
      "org.jfree.data.general.Dataset",
      "org.eclipse.sisu.space.ResourceEnumeration$NestedJarConnection",
      "com.fasterxml.jackson.core.type.ResolvedType",
      "com.google.inject.internal.MoreTypes$CompositeType",
      "org.xwiki.extension.repository.DefaultExtensionRepositoryDescriptor",
      "org.xwiki.observation.event.AllEvent",
      "org.xwiki.extension.AbstractExtensionScm",
      "org.infinispan.commons.marshall.MarshallableFunctions$LambdaWithMetas",
      "org.apache.hadoop.HadoopIllegalArgumentException",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasReturnPrevOrNull",
      "org.xwiki.extension.job.AbstractExtensionRequest",
      "org.infinispan.commons.marshall.MarshallableFunctions$ReturnReadWriteGet",
      "org.infinispan.stream.StreamMarshalling$NonNullPredicate",
      "com.google.common.collect.MapMakerInternalMap$ReferenceEntry",
      "org.apache.hadoop.util.Shell$ShellCommandExecutor",
      "org.jfree.data.xy.WindDataset",
      "info.informatica.io.FilePatternSpec",
      "com.google.common.reflect.Types$JavaVersion",
      "org.xwiki.component.descriptor.DefaultComponentDependency",
      "org.xwiki.extension.LocalExtension",
      "org.jfree.data.statistics.SimpleHistogramDataset",
      "org.jfree.data.time.SimpleTimePeriod",
      "org.infinispan.stream.impl.TerminalFunctions$ReduceLongFunction",
      "org.jfree.data.xy.XYIntervalSeriesCollection",
      "org.xwiki.observation.event.ApplicationStartedEvent",
      "org.xwiki.extension.job.plan.ExtensionPlanNode",
      "org.jfree.util.PublicCloneable",
      "org.jfree.data.xy.DefaultXYZDataset",
      "org.apache.hadoop.fs.FsUrlStreamHandlerFactory",
      "com.google.common.base.FunctionalEquivalence",
      "org.xwiki.extension.version.internal.DefaultVersionConstraint",
      "org.xwiki.extension.job.plan.ExtensionPlanAction",
      "org.jfree.data.general.AbstractDataset",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayIntFunction",
      "org.xwiki.extension.job.plan.ExtensionPlanTree",
      "org.xwiki.extension.version.InvalidVersionConstraintException",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayDoubleFunction",
      "org.infinispan.stream.impl.TerminalFunctions$MinIntFunction",
      "org.xwiki.extension.ExtensionScmConnection",
      "org.xwiki.extension.InstalledExtension",
      "org.jfree.util.SortOrder",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueReturnView",
      "org.xwiki.extension.ExtensionIssueManagement",
      "org.xwiki.extension.job.plan.internal.DefaultExtensionPlanNode",
      "org.jfree.data.statistics.DefaultBoxAndWhiskerCategoryDataset",
      "org.jfree.data.category.SlidingCategoryDataset",
      "org.jfree.data.UnknownKeyException",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueIfPresentReturnPrevOrNull",
      "ucar.nc2.util.net.URLStreamHandlerFactory",
      "org.jfree.data.time.TimePeriod",
      "org.xwiki.job.Request",
      "org.xwiki.extension.job.UninstallRequest",
      "org.infinispan.stream.impl.TerminalFunctions$AverageLongFunction",
      "org.jfree.data.statistics.HistogramDataset",
      "org.apache.commons.vfs2.provider.DefaultURLStreamHandler",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueIfAbsentReturnBoolean",
      "org.apache.hadoop.conf.Configuration$DeprecationDelta",
      "org.xwiki.extension.AbstractExtensionIssueManagement",
      "org.infinispan.stream.impl.TerminalFunctions$SumDoubleFunction",
      "org.xwiki.component.descriptor.ComponentDependency",
      "org.apache.hadoop.conf.Configuration$DeprecatedKeyInfo",
      "org.apache.hadoop.fs.FilterFileSystem",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfPresentReturnBoolean",
      "org.jfree.data.general.PieDataset",
      "com.fasterxml.jackson.databind.JavaType",
      "org.apache.hadoop.util.StringInterner",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfPresentReturnPrevOrNull",
      "org.jfree.data.xy.XYRangeInfo",
      "org.xwiki.job.Job",
      "org.xwiki.extension.version.VersionConstraint",
      "org.infinispan.iteration.impl.DistributedEntryRetriever$MapAction",
      "com.google.common.reflect.Types$GenericArrayTypeImpl",
      "org.xwiki.extension.job.InstallRequest",
      "org.jfree.data.general.KeyedValues2DDataset",
      "org.infinispan.stream.impl.TerminalFunctions$CollectLongFunction",
      "org.apache.commons.vfs2.FileSystemOptions$FileSystemOptionKey",
      "org.xwiki.extension.job.internal.AbstractExtensionJob",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueMetasReturnView",
      "info.informatica.io.WildcardFilter",
      "org.apache.hadoop.classification.InterfaceAudience$LimitedPrivate",
      "org.xwiki.extension.DefaultExtensionIssueManagement",
      "org.jfree.data.statistics.DefaultMultiValueCategoryDataset",
      "org.jfree.data.Range",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchLongFunction",
      "org.jfree.data.statistics.DefaultStatisticalCategoryDataset",
      "org.jfree.data.category.IntervalCategoryDataset",
      "org.apache.commons.dbcp2.PoolingConnection",
      "org.jfree.data.time.Year",
      "org.apache.commons.pool2.PooledObject",
      "org.apache.hadoop.util.Shell",
      "org.infinispan.stream.impl.TerminalFunctions$ToArrayFunction",
      "org.xwiki.component.descriptor.DefaultComponentDescriptor",
      "org.infinispan.stream.impl.TerminalFunctions$NoneMatchFunction",
      "org.xwiki.text.XWikiToStringBuilder",
      "org.xwiki.observation.event.AbstractDocumentEvent",
      "org.infinispan.stream.impl.TerminalFunctions$IdentityReduceLongFunction",
      "org.xwiki.classloader.internal.ExtendedURLStreamHandlerFactory",
      "org.apache.hadoop.conf.Configured",
      "org.jfree.data.time.TimePeriodValuesCollection",
      "org.jfree.data.xy.XIntervalDataItem",
      "org.infinispan.stream.StreamMarshalling$EntryToKeyFunction",
      "org.xwiki.component.manager.ComponentEventManager",
      "org.infinispan.commons.marshall.MarshallableFunctions$ReturnReadWriteFind",
      "com.google.gson.internal.$Gson$Types$WildcardTypeImpl",
      "org.infinispan.stream.impl.TerminalFunctions$MaxFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$AbstractSetValueReturnView",
      "org.apache.hadoop.util.Shell$ShellTimeoutTimerTask",
      "org.xwiki.extension.repository.ExtensionRepositoryDescriptor",
      "org.jfree.data.xy.TableXYDataset",
      "org.xwiki.extension.LocalExtensionFile",
      "org.xwiki.extension.DefaultExtensionScmConnection",
      "org.xwiki.extension.version.internal.DefaultVersion",
      "org.xwiki.extension.repository.ExtensionRepository",
      "org.jfree.data.gantt.GanttCategoryDataset",
      "org.xwiki.observation.WrappedThreadEventListener",
      "org.jfree.data.jdbc.JDBCCategoryDataset",
      "org.jfree.data.Values2D",
      "org.apache.commons.lang3.builder.ToStringBuilder",
      "org.jfree.data.statistics.BoxAndWhiskerXYDataset",
      "org.jfree.data.general.DatasetChangeEvent",
      "org.jfree.data.general.SeriesChangeListener",
      "org.infinispan.commons.marshall.MarshallableFunctions$RemoveReturnBoolean",
      "org.apache.commons.dbcp2.PoolingDataSource$PoolGuardConnectionWrapper",
      "org.jfree.data.category.DefaultCategoryDataset",
      "org.infinispan.stream.impl.TerminalFunctions$AllMatchDoubleFunction",
      "org.jfree.data.general.KeyedValuesDataset",
      "org.infinispan.stream.impl.TerminalFunctions$FindAnyFunction",
      "org.jfree.data.time.Week",
      "org.jfree.data.xy.VectorSeries",
      "org.jfree.data.general.DefaultKeyedValuesDataset",
      "org.xwiki.observation.AbstractEventListener",
      "org.jfree.data.general.Series",
      "org.infinispan.stream.impl.DistributedCacheStream$HandOffConsumer",
      "org.apache.hadoop.conf.Configuration$Resource",
      "com.fasterxml.jackson.databind.type.TypeBindings",
      "com.google.common.base.PairwiseEquivalence",
      "org.jfree.data.xy.XYSeriesCollection",
      "org.apache.hadoop.classification.InterfaceStability$Unstable",
      "org.apache.commons.dbcp2.DelegatingCallableStatement",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValue",
      "org.jfree.util.TableOrder",
      "org.xwiki.extension.Extension",
      "org.jfree.data.KeyedValue",
      "org.xwiki.job.AbstractRequest",
      "org.infinispan.stream.impl.TerminalFunctions$MaxDoubleFunction",
      "org.jfree.data.xy.IntervalXYDelegate",
      "org.infinispan.filter.CacheFilters$FilterConverterAsCacheEntryFunction",
      "org.infinispan.commons.marshall.MarshallableFunctions$SetValueIfEqualsReturnBoolean",
      "org.jfree.data.KeyedObjects2D",
      "org.apache.commons.lang3.builder.Builder",
      "org.apache.hadoop.util.Time",
      "org.apache.hadoop.fs.LocalFileSystem",
      "org.osgi.service.url.AbstractURLStreamHandlerService"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("java.util.function.Consumer", false, AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest_scaffolding.class.getClassLoader()));
  }
}
