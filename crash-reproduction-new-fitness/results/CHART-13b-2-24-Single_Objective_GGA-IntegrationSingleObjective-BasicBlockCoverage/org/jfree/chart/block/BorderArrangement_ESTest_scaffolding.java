/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon May 18 01:34:41 UTC 2020
 */

package org.jfree.chart.block;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class BorderArrangement_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.jfree.chart.block.BorderArrangement"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(BorderArrangement_ESTest_scaffolding.class.getClassLoader() ,
      "org.jfree.data.time.TimeTableXYDataset",
      "org.jfree.chart.renderer.xy.VectorRenderer",
      "org.jfree.chart.labels.StandardPieSectionLabelGenerator",
      "com.lowagie.text.pdf.TrueTypeFontUnicode",
      "org.jfree.chart.plot.ValueMarker",
      "org.jfree.chart.renderer.xy.DeviationRenderer$State",
      "org.jfree.chart.labels.BubbleXYItemLabelGenerator",
      "org.jfree.data.resources.DataPackageResources",
      "com.lowagie.text.Jpeg2000",
      "org.jfree.data.time.Minute",
      "com.lowagie.text.pdf.PdfOCG",
      "org.jfree.chart.Effect3D",
      "org.jfree.chart.renderer.xy.XYAreaRenderer",
      "com.lowagie.text.Element",
      "org.jfree.data.general.ValueDataset",
      "com.lowagie.text.pdf.collection.PdfCollectionItem",
      "org.jfree.chart.urls.CustomPieURLGenerator",
      "com.lowagie.text.Annotation",
      "org.jfree.data.general.AbstractSeriesDataset",
      "org.jfree.chart.LegendItemCollection",
      "org.jfree.chart.annotations.XYShapeAnnotation",
      "org.jfree.data.xy.DefaultTableXYDataset",
      "com.lowagie.text.pdf.ExtraEncoding",
      "org.jfree.data.RangeInfo",
      "org.jfree.data.general.CombinedDataset",
      "org.jfree.chart.renderer.xy.XYBoxAndWhiskerRenderer",
      "org.jfree.chart.text.TextFragment",
      "com.lowagie.text.pdf.ColumnText",
      "com.lowagie.text.pdf.CJKFont",
      "com.lowagie.text.pdf.DefaultFontMapper$BaseFontParameters",
      "com.lowagie.text.pdf.PdfDocument$RenderingContext",
      "com.lowagie.text.GreekList",
      "org.jfree.data.jdbc.JDBCXYDataset",
      "org.jfree.chart.labels.ItemLabelPosition",
      "org.jfree.chart.renderer.category.GroupedStackedBarRenderer",
      "org.jfree.data.statistics.HistogramType",
      "com.lowagie.text.pdf.internal.PdfAnnotationsImp",
      "com.lowagie.text.pdf.AsianFontMapper",
      "org.jfree.data.xy.DefaultXYDataset",
      "org.jfree.chart.plot.PiePlot3D",
      "org.jfree.chart.util.PaintList",
      "org.jfree.chart.labels.ItemLabelAnchor",
      "org.jfree.chart.labels.StandardXYToolTipGenerator",
      "org.jfree.chart.labels.BoxAndWhiskerToolTipGenerator",
      "org.jfree.chart.ChartMouseEvent",
      "com.lowagie.text.pdf.PdfReader$PageRefs",
      "org.jfree.chart.axis.Timeline",
      "org.jfree.chart.labels.XYZToolTipGenerator",
      "org.jfree.chart.ui.Contributor",
      "org.jfree.data.time.TimeSeriesCollection",
      "org.jfree.chart.needle.LongNeedle",
      "com.lowagie.text.pdf.PdfSigGenericPKCS$PPKLite",
      "com.lowagie.text.pdf.PdfStructureElement",
      "org.jfree.chart.util.RectangleAnchor",
      "com.lowagie.text.ZapfDingbatsNumberList",
      "com.lowagie.text.pdf.PRTokeniser",
      "org.jfree.data.xy.DefaultHighLowDataset",
      "org.jfree.chart.entity.XYAnnotationEntity",
      "org.jfree.data.time.TimePeriodFormatException",
      "org.jfree.chart.renderer.xy.XYDifferenceRenderer",
      "org.jfree.chart.text.TextBlock",
      "com.lowagie.text.pdf.ColorDetails",
      "org.jfree.chart.axis.Axis",
      "com.lowagie.text.html.Markup",
      "com.lowagie.text.pdf.PdfPattern",
      "com.lowagie.text.pdf.PdfRectangle",
      "org.jfree.chart.axis.NumberAxis3D",
      "org.jfree.chart.renderer.category.StatisticalLineAndShapeRenderer",
      "org.jfree.chart.block.BlockBorder",
      "org.jfree.chart.annotations.XYImageAnnotation",
      "org.jfree.chart.util.UnitType",
      "com.lowagie.text.pdf.PdfPageEvent",
      "org.jfree.chart.annotations.XYAnnotation",
      "org.jfree.chart.renderer.xy.CyclicXYItemRenderer",
      "org.jfree.chart.renderer.xy.XYLine3DRenderer",
      "org.jfree.chart.util.ObjectUtilities",
      "org.jfree.chart.needle.MeterNeedle",
      "org.jfree.chart.renderer.xy.XYBarRenderer$XYBarRendererState",
      "org.jfree.data.time.Second",
      "org.jfree.data.xy.YIntervalDataItem",
      "com.lowagie.text.pdf.PdfStamperImp",
      "org.jfree.data.xy.XIntervalSeries",
      "com.lowagie.text.pdf.PdfOutline",
      "org.jfree.chart.needle.PlumNeedle",
      "com.lowagie.text.ElementTags",
      "org.jfree.chart.plot.DatasetRenderingOrder",
      "org.jfree.chart.event.RendererChangeListener",
      "org.jfree.chart.block.FlowArrangement",
      "org.jfree.data.KeyedValues",
      "org.jfree.data.xy.XYIntervalSeries",
      "org.jfree.chart.plot.CombinedRangeCategoryPlot",
      "com.lowagie.text.RomanList",
      "org.jfree.chart.title.CompositeTitle",
      "org.jfree.chart.labels.StandardCategoryToolTipGenerator",
      "com.lowagie.text.Utilities",
      "org.jfree.chart.event.TitleChangeListener",
      "org.jfree.chart.text.TextMeasurer",
      "org.jfree.chart.renderer.xy.XYAreaRenderer2",
      "org.jfree.chart.LegendItemSource",
      "com.lowagie.text.pdf.hyphenation.Hyphenator",
      "com.lowagie.text.pdf.PdfAction",
      "org.jfree.chart.imagemap.StandardURLTagFragmentGenerator",
      "org.jfree.chart.renderer.PolarItemRenderer",
      "org.jfree.data.xy.XYZDataset",
      "com.lowagie.text.FontFactory",
      "org.jfree.chart.renderer.AreaRendererEndType",
      "org.jfree.chart.plot.PieLabelLinkStyle",
      "org.jfree.data.general.DatasetChangeListener",
      "org.jfree.data.statistics.MultiValueCategoryDataset",
      "org.jfree.data.gantt.TaskSeries",
      "org.jfree.data.xy.YIntervalSeries",
      "org.jfree.chart.renderer.category.BarRenderer3D",
      "org.jfree.chart.event.TitleChangeEvent",
      "org.jfree.chart.ui.ExtensionFileFilter",
      "org.jfree.chart.needle.PointerNeedle",
      "org.jfree.chart.renderer.category.WaterfallBarRenderer",
      "org.jfree.chart.ui.Licences",
      "org.jfree.chart.block.EmptyBlock",
      "org.jfree.data.statistics.SimpleHistogramBin",
      "org.jfree.chart.axis.CyclicNumberAxis$CycleBoundTick",
      "com.lowagie.text.pdf.Type3Font",
      "org.jfree.chart.event.ChartChangeEventType",
      "com.lowagie.text.pdf.PdfNull",
      "com.lowagie.text.pdf.PdfOCProperties",
      "org.jfree.data.time.Millisecond",
      "com.lowagie.text.pdf.PdfWriter$PdfBody",
      "org.jfree.chart.axis.ExtendedCategoryAxis",
      "org.jfree.chart.util.GradientPaintTransformType",
      "com.lowagie.text.pdf.PdfDocument",
      "com.lowagie.text.pdf.ByteBuffer",
      "org.jfree.chart.renderer.category.CategoryItemRenderer",
      "com.lowagie.text.pdf.PdfBorderDictionary",
      "com.lowagie.text.pdf.PdfSigGenericPKCS$VeriSign",
      "org.jfree.chart.util.Size2D",
      "org.jfree.chart.annotations.CategoryTextAnnotation",
      "org.jfree.chart.renderer.category.LineAndShapeRenderer",
      "com.lowagie.text.SimpleCell",
      "org.jfree.chart.util.ShapeUtilities",
      "org.jfree.data.general.DefaultKeyedValueDataset",
      "com.lowagie.text.pdf.PdfMediaClipData",
      "org.jfree.data.general.SeriesChangeEvent",
      "org.jfree.chart.ChartPanel",
      "org.jfree.chart.plot.FastScatterPlot",
      "org.jfree.chart.labels.CategoryItemLabelGenerator",
      "org.jfree.chart.renderer.xy.StackedXYAreaRenderer$StackedXYAreaRendererState",
      "org.jfree.chart.axis.TickUnits",
      "com.lowagie.text.pdf.PdfWriter",
      "com.lowagie.text.pdf.PdfLiteral",
      "com.lowagie.text.pdf.PdfPrinterGraphics2D",
      "org.jfree.chart.PolarChartPanel",
      "com.lowagie.text.pdf.collection.PdfCollectionField",
      "org.jfree.chart.imagemap.URLTagFragmentGenerator",
      "com.lowagie.text.pdf.GrayColor",
      "org.jfree.chart.util.VerticalAlignment",
      "org.jfree.chart.renderer.category.GanttRenderer",
      "org.jfree.chart.renderer.category.StackedBarRenderer",
      "org.jfree.chart.plot.SeriesRenderingOrder",
      "org.jfree.chart.renderer.category.StackedAreaRenderer",
      "org.jfree.chart.block.EntityBlockResult",
      "org.jfree.chart.urls.StandardXYZURLGenerator",
      "com.lowagie.text.pdf.PdfXConformanceException",
      "org.jfree.chart.axis.NumberTick",
      "com.lowagie.text.Phrase",
      "com.lowagie.text.pdf.events.FieldPositioningEvents",
      "org.jfree.chart.renderer.category.CategoryStepRenderer$State",
      "org.jfree.chart.block.BlockContainer",
      "org.jfree.data.xy.DefaultWindDataset",
      "org.jfree.chart.renderer.xy.StandardXYItemRenderer",
      "org.jfree.chart.plot.Marker",
      "org.jfree.chart.ChartMouseListener",
      "org.jfree.data.xy.IntervalXYZDataset",
      "com.lowagie.text.pdf.PdfImportedPage",
      "org.jfree.chart.renderer.category.CategoryItemRendererState",
      "org.jfree.data.time.FixedMillisecond",
      "org.jfree.chart.renderer.category.DefaultCategoryItemRenderer",
      "org.jfree.chart.axis.SegmentedTimeline",
      "com.lowagie.text.Paragraph",
      "org.jfree.chart.axis.ValueAxis",
      "org.jfree.chart.needle.ShipNeedle",
      "org.jfree.data.Value",
      "org.jfree.data.time.TimePeriodAnchor",
      "org.jfree.chart.plot.MultiplePiePlot",
      "org.jfree.data.gantt.Task",
      "org.jfree.data.general.junit.DefaultHeatMapDatasetTests",
      "org.jfree.chart.block.BlockFrame",
      "com.lowagie.text.Section",
      "org.jfree.data.general.DefaultValueDataset",
      "com.lowagie.text.Cell",
      "com.lowagie.text.pdf.PdfPCell",
      "com.lowagie.text.pdf.ExtendedColor",
      "com.lowagie.text.pdf.PdfFunction",
      "com.lowagie.text.pdf.PdfDocument$PdfInfo",
      "org.jfree.data.xy.VectorSeriesCollection",
      "org.jfree.chart.renderer.xy.AbstractXYItemRenderer",
      "org.jfree.chart.plot.CrosshairState",
      "org.jfree.chart.labels.XYSeriesLabelGenerator",
      "org.jfree.chart.urls.TimeSeriesURLGenerator",
      "org.jfree.chart.renderer.category.StackedBarRenderer3D",
      "com.lowagie.text.pdf.PdfWriter$PdfTrailer",
      "org.jfree.chart.plot.PieLabelRecord",
      "org.jfree.chart.block.ColorBlock",
      "org.jfree.data.DomainOrder",
      "org.jfree.chart.axis.AxisState",
      "org.jfree.chart.renderer.AbstractRenderer",
      "com.lowagie.text.ChapterAutoNumber",
      "org.jfree.chart.renderer.xy.XYStepRenderer",
      "com.lowagie.text.pdf.PdfTransparencyGroup",
      "org.jfree.chart.plot.RingPlot",
      "org.jfree.chart.renderer.xy.HighLowRenderer",
      "org.jfree.chart.block.RectangleConstraint",
      "org.jfree.chart.renderer.DefaultPolarItemRenderer",
      "com.lowagie.text.pdf.PdfBorderArray",
      "org.jfree.chart.title.Title",
      "com.lowagie.text.pdf.PdfEncodings$SymbolConversion",
      "org.jfree.data.jdbc.JDBCPieDataset",
      "org.jfree.chart.labels.PieToolTipGenerator",
      "org.jfree.chart.block.LineBorder",
      "org.jfree.chart.axis.DateTickMarkPosition",
      "com.lowagie.text.pdf.PdfFormXObject",
      "org.jfree.chart.urls.StandardPieURLGenerator",
      "org.jfree.data.DefaultKeyedValues2D",
      "com.lowagie.text.pdf.FontDetails",
      "org.jfree.data.xy.DefaultOHLCDataset",
      "com.lowagie.text.pdf.PdfShading",
      "org.jfree.chart.renderer.xy.StandardXYItemRenderer$State",
      "org.jfree.chart.block.BorderArrangement",
      "org.jfree.chart.plot.CombinedDomainXYPlot",
      "com.lowagie.text.pdf.PdfEncodings$Cp437Conversion",
      "org.jfree.data.time.Day",
      "org.jfree.data.general.SeriesDataset",
      "com.lowagie.text.pdf.PdfReader",
      "com.lowagie.text.pdf.Type3Glyph",
      "org.jfree.chart.ui.BasicProjectInfo",
      "org.jfree.data.xy.XYDataset",
      "com.lowagie.text.ZapfDingbatsList",
      "org.jfree.chart.block.Arrangement",
      "org.jfree.chart.plot.Plot",
      "com.lowagie.text.pdf.IntHashtable$Entry",
      "org.jfree.chart.plot.ValueAxisPlot",
      "org.jfree.chart.renderer.category.IntervalBarRenderer",
      "org.jfree.chart.renderer.category.BarRenderer",
      "org.jfree.chart.event.AxisChangeEvent",
      "org.jfree.chart.LegendItem",
      "org.jfree.chart.renderer.xy.XYErrorRenderer",
      "org.jfree.chart.renderer.xy.StackedXYAreaRenderer",
      "org.jfree.chart.urls.CategoryURLGenerator",
      "org.jfree.chart.plot.PlotRenderingInfo",
      "org.jfree.chart.renderer.xy.CandlestickRenderer",
      "org.jfree.data.xy.AbstractXYZDataset",
      "org.jfree.data.time.RegularTimePeriod",
      "org.jfree.chart.entity.TickLabelEntity",
      "org.jfree.data.time.TimeSeries",
      "com.lowagie.text.pdf.PdfPageLabels",
      "org.jfree.chart.renderer.category.LineRenderer3D",
      "com.lowagie.text.pdf.PdfDashPattern",
      "org.jfree.chart.renderer.xy.StackedXYAreaRenderer2",
      "org.jfree.chart.entity.CategoryItemEntity",
      "org.jfree.data.general.CombinationDataset",
      "org.jfree.chart.entity.AxisLabelEntity",
      "org.jfree.chart.plot.AbstractPieLabelDistributor",
      "org.jfree.data.general.DatasetUtilities",
      "org.jfree.chart.util.BooleanList",
      "com.lowagie.text.pdf.PdfDictionary",
      "org.jfree.chart.annotations.TextAnnotation",
      "org.jfree.chart.util.AbstractObjectList",
      "org.jfree.chart.labels.StandardCategorySeriesLabelGenerator",
      "org.jfree.data.xy.IntervalXYDataset",
      "org.jfree.chart.renderer.xy.XYLineAndShapeRenderer",
      "org.jfree.chart.needle.LineNeedle",
      "org.jfree.data.xy.Vector",
      "org.jfree.data.general.DefaultPieDataset",
      "org.jfree.chart.entity.CategoryLabelEntity",
      "org.jfree.chart.util.RectangleEdge",
      "org.jfree.chart.plot.DefaultDrawingSupplier",
      "org.jfree.data.category.DefaultIntervalCategoryDataset",
      "org.jfree.chart.plot.PieLabelDistributor",
      "org.jfree.data.xy.XIntervalSeriesCollection",
      "com.lowagie.text.ElementListener",
      "org.jfree.data.category.CategoryDataset",
      "org.jfree.data.ComparableObjectItem",
      "org.jfree.data.xy.XYSeries",
      "com.lowagie.text.pdf.HyphenationAuto",
      "com.lowagie.text.pdf.EnumerateTTC",
      "org.jfree.chart.util.RectangleInsets",
      "org.jfree.chart.text.TextAnchor",
      "org.jfree.chart.labels.AbstractCategoryItemLabelGenerator",
      "org.jfree.chart.axis.TickUnit",
      "org.jfree.chart.ChartColor",
      "org.jfree.chart.util.GradientPaintTransformer",
      "org.jfree.data.gantt.TaskSeriesCollection",
      "com.lowagie.text.pdf.internal.PdfVersionImp",
      "org.jfree.chart.annotations.CategoryPointerAnnotation",
      "com.lowagie.text.pdf.PdfImage",
      "org.jfree.chart.util.StrokeList",
      "com.lowagie.text.pdf.interfaces.PdfVersion",
      "org.jfree.chart.util.StandardGradientPaintTransformer",
      "com.lowagie.text.pdf.internal.PdfViewerPreferencesImp",
      "org.jfree.chart.text.TextUtilities",
      "org.jfree.chart.axis.CategoryAxis3D",
      "com.lowagie.text.pdf.PdfPage",
      "org.jfree.chart.axis.TickUnitSource",
      "org.jfree.chart.plot.CategoryMarker",
      "org.jfree.data.time.SpreadsheetDate",
      "com.lowagie.text.pdf.fonts.FontsResourceAnchor",
      "org.jfree.chart.imagemap.OverLIBToolTipTagFragmentGenerator",
      "org.jfree.data.xy.MatrixSeriesCollection",
      "org.jfree.chart.block.AbstractBlock",
      "org.jfree.chart.plot.XYPlot",
      "com.lowagie.text.pdf.PdfName",
      "org.jfree.chart.util.TableOrder",
      "org.jfree.chart.annotations.AbstractXYAnnotation",
      "org.jfree.data.xy.MatrixSeries",
      "org.jfree.chart.title.ImageTitle",
      "com.lowagie.text.pdf.PdfGState",
      "com.lowagie.text.List",
      "com.lowagie.text.pdf.RandomAccessFileOrArray",
      "org.jfree.chart.labels.StandardPieToolTipGenerator",
      "org.jfree.chart.renderer.xy.WindItemRenderer",
      "com.lowagie.text.ImgWMF",
      "org.jfree.chart.labels.SymbolicXYItemLabelGenerator",
      "org.jfree.data.time.TimeSeriesDataItem",
      "com.lowagie.text.pdf.PdfAppearance",
      "org.jfree.chart.annotations.XYLineAnnotation",
      "com.lowagie.text.pdf.PdfTextArray",
      "org.jfree.chart.renderer.category.MinMaxCategoryRenderer",
      "org.jfree.chart.plot.SpiderWebPlot",
      "org.jfree.chart.renderer.xy.XYItemRendererState",
      "com.lowagie.text.Jpeg",
      "org.jfree.data.xy.XYBarDataset",
      "com.lowagie.text.pdf.PdfLayer",
      "org.jfree.chart.plot.ThermometerPlot",
      "com.lowagie.text.ImgRaw",
      "org.jfree.data.xy.VectorXYDataset",
      "org.jfree.data.time.Hour",
      "com.lowagie.text.pdf.PdfPTableEvent",
      "org.jfree.chart.title.PaintScaleLegend",
      "org.jfree.chart.axis.PeriodAxisLabelInfo",
      "org.jfree.chart.labels.AbstractXYItemLabelGenerator",
      "org.jfree.chart.axis.SubCategoryAxis",
      "com.lowagie.text.pdf.collection.PdfTargetDictionary",
      "org.jfree.chart.axis.NumberTickUnit",
      "com.lowagie.text.pdf.TrueTypeFont",
      "org.jfree.chart.imagemap.DynamicDriveToolTipTagFragmentGenerator",
      "org.jfree.chart.title.TextTitle",
      "com.lowagie.text.factories.ElementFactory",
      "com.lowagie.text.pdf.PdfReaderInstance",
      "com.lowagie.text.pdf.PRIndirectReference",
      "org.jfree.data.statistics.StatisticalCategoryDataset",
      "org.jfree.chart.labels.IntervalCategoryItemLabelGenerator",
      "com.lowagie.text.DocumentException",
      "com.lowagie.text.Font",
      "org.jfree.chart.event.ChartChangeEvent",
      "com.lowagie.text.pdf.PdfPSXObject",
      "org.jfree.data.Values",
      "org.jfree.data.KeyedValues2D",
      "org.jfree.chart.axis.CyclicNumberAxis",
      "org.jfree.chart.text.TextBlockAnchor",
      "com.lowagie.text.pdf.PRAcroForm",
      "org.jfree.chart.needle.MiddlePinNeedle",
      "com.lowagie.text.PageSize",
      "com.lowagie.text.pdf.AcroFields",
      "org.jfree.data.general.SeriesException",
      "org.jfree.chart.event.ChartChangeListener",
      "org.jfree.data.general.CombinedDataset$DatasetInfo",
      "org.jfree.chart.renderer.category.LevelRenderer",
      "org.jfree.chart.event.ChartProgressListener",
      "org.jfree.chart.renderer.xy.XYDotRenderer",
      "org.jfree.chart.labels.AbstractPieItemLabelGenerator",
      "org.jfree.chart.text.TextBox",
      "org.jfree.chart.plot.WaferMapPlot",
      "org.jfree.chart.labels.CategorySeriesLabelGenerator",
      "org.jfree.chart.needle.WindNeedle",
      "org.jfree.data.general.SubSeriesDataset",
      "com.lowagie.text.pdf.interfaces.PdfViewerPreferences",
      "com.lowagie.text.pdf.PdfEncodings$SymbolTTConversion",
      "com.lowagie.text.pdf.PdfPCellEvent",
      "org.jfree.chart.axis.MarkerAxisBand",
      "com.lowagie.text.xml.xmp.XmpWriter",
      "com.lowagie.text.pdf.PdfPatternPainter",
      "org.jfree.chart.axis.CategoryAxis",
      "org.jfree.chart.renderer.xy.XYItemRenderer",
      "org.jfree.data.xy.AbstractIntervalXYDataset",
      "org.jfree.chart.ui.ProjectInfo",
      "org.jfree.chart.renderer.RendererState",
      "org.jfree.data.xy.OHLCDataItem",
      "org.jfree.chart.entity.StandardEntityCollection",
      "com.lowagie.text.pdf.PdfReader$1",
      "com.lowagie.text.pdf.PdfPageEventHelper",
      "com.lowagie.text.Anchor",
      "org.jfree.chart.axis.LogarithmicAxis",
      "org.jfree.chart.plot.CompassPlot",
      "com.lowagie.text.pdf.PdfBoolean",
      "org.jfree.chart.axis.DateAxis$DefaultTimeline",
      "org.jfree.data.xy.YIntervalSeriesCollection",
      "com.lowagie.text.TextElementArray",
      "com.lowagie.text.SimpleTable",
      "com.lowagie.text.pdf.PdfChunk",
      "org.jfree.data.xy.AbstractXYDataset",
      "org.jfree.chart.labels.StandardXYSeriesLabelGenerator",
      "com.lowagie.text.Row",
      "org.jfree.chart.axis.DateAxis",
      "org.jfree.chart.annotations.XYPointerAnnotation",
      "org.jfree.chart.plot.CategoryPlot",
      "org.jfree.chart.event.MarkerChangeListener",
      "com.lowagie.text.pdf.interfaces.PdfPageActions",
      "org.jfree.chart.event.PlotChangeEvent",
      "org.jfree.chart.plot.PiePlot",
      "com.lowagie.text.pdf.PageResources",
      "com.lowagie.text.HeaderFooter",
      "org.jfree.chart.annotations.XYPolygonAnnotation",
      "org.jfree.data.xy.DefaultIntervalXYDataset",
      "org.jfree.chart.renderer.category.LayeredBarRenderer",
      "org.jfree.chart.event.RendererChangeEvent",
      "org.jfree.chart.text.G2TextMeasurer",
      "com.lowagie.text.pdf.PdfException",
      "org.jfree.chart.block.CenterArrangement",
      "com.lowagie.text.pdf.PdfIndirectReference",
      "com.lowagie.text.pdf.PdfPTable",
      "org.jfree.chart.labels.PieSectionLabelGenerator",
      "org.jfree.chart.axis.Tick",
      "com.lowagie.text.pdf.DocumentFont",
      "org.jfree.data.statistics.BoxAndWhiskerItem",
      "org.jfree.data.category.CategoryToPieDataset",
      "org.jfree.chart.urls.PieURLGenerator",
      "org.jfree.data.DefaultKeyedValues",
      "com.lowagie.text.pdf.PdfArray",
      "org.jfree.data.statistics.DefaultBoxAndWhiskerXYDataset",
      "org.jfree.data.statistics.BoxAndWhiskerCategoryDataset",
      "org.jfree.chart.util.ShapeList",
      "org.jfree.chart.labels.StandardXYItemLabelGenerator",
      "com.lowagie.text.pdf.collection.PdfCollection",
      "org.jfree.chart.urls.XYURLGenerator",
      "org.jfree.data.time.TimePeriodValues",
      "org.jfree.chart.annotations.XYDrawableAnnotation",
      "com.lowagie.text.pdf.OutputStreamCounter",
      "org.jfree.chart.util.HorizontalAlignment",
      "org.jfree.chart.imagemap.StandardToolTipTagFragmentGenerator",
      "com.lowagie.text.DocWriter",
      "org.jfree.chart.urls.CustomXYURLGenerator",
      "org.jfree.chart.axis.CategoryAnchor",
      "org.jfree.data.time.DynamicTimeSeriesCollection$ValueSequence",
      "org.jfree.chart.axis.SegmentedTimeline$BaseTimelineSegmentRange",
      "org.jfree.data.xy.CategoryTableXYDataset",
      "org.jfree.chart.labels.CategoryToolTipGenerator",
      "org.jfree.chart.renderer.xy.XYStepAreaRenderer",
      "org.jfree.data.general.DefaultKeyedValues2DDataset",
      "com.lowagie.text.pdf.PdfSpotColor",
      "org.jfree.chart.entity.XYItemEntity",
      "com.lowagie.text.Header",
      "com.lowagie.text.ListItem",
      "org.jfree.data.time.Quarter",
      "org.jfree.chart.plot.DialShape",
      "org.jfree.chart.axis.CategoryLabelPositions",
      "org.jfree.data.time.TimePeriodValue",
      "com.lowagie.text.pdf.PdfTransition",
      "org.jfree.data.time.DynamicTimeSeriesCollection",
      "org.jfree.chart.util.SortOrder",
      "org.jfree.data.DomainInfo",
      "org.jfree.chart.labels.StandardXYZToolTipGenerator",
      "com.lowagie.text.pdf.PdfPages",
      "org.jfree.chart.renderer.xy.XYSplineAndShapeRenderer",
      "org.jfree.chart.block.BlockParams",
      "com.lowagie.text.MarkedSection",
      "org.jfree.chart.annotations.CategoryAnnotation",
      "org.jfree.chart.axis.AxisSpace",
      "org.jfree.chart.axis.SegmentedTimeline$SegmentRange",
      "org.jfree.chart.plot.PolarPlot",
      "com.lowagie.text.FontFactoryImp",
      "org.jfree.data.ComparableObjectSeries",
      "org.jfree.chart.plot.MeterInterval",
      "org.jfree.data.xy.XYDataItem",
      "org.jfree.data.time.DateRange",
      "org.jfree.data.general.DatasetGroup",
      "org.jfree.data.DefaultKeyedValue",
      "org.jfree.data.general.KeyedValueDataset",
      "org.jfree.chart.util.Rotation",
      "org.jfree.data.xy.XYIntervalDataItem",
      "org.jfree.chart.block.BlockResult",
      "org.jfree.data.xy.OHLCDataset",
      "org.jfree.data.xy.VectorDataItem",
      "com.lowagie.text.pdf.PdfAnnotation",
      "com.lowagie.text.pdf.PdfSignature",
      "com.lowagie.text.pdf.PdfSigGenericPKCS",
      "org.jfree.chart.renderer.xy.XYLineAndShapeRenderer$State",
      "org.jfree.chart.resources.JFreeChartResources",
      "org.jfree.chart.renderer.xy.XYBlockRenderer",
      "org.jfree.data.RangeType",
      "com.lowagie.text.pdf.PdfStructureTreeRoot",
      "org.jfree.data.time.Month",
      "org.jfree.chart.renderer.category.CategoryStepRenderer",
      "org.jfree.chart.title.DateTitle",
      "org.jfree.chart.renderer.LookupPaintScale",
      "org.jfree.data.general.WaferMapDataset",
      "org.jfree.chart.renderer.category.ScatterRenderer",
      "org.jfree.chart.block.EntityBlockParams",
      "org.jfree.data.general.Dataset",
      "org.jfree.chart.block.LabelBlock",
      "org.jfree.chart.plot.PlotOrientation",
      "org.jfree.chart.axis.NumberAxis",
      "com.lowagie.text.DocListener",
      "com.lowagie.text.pdf.events.PdfPTableEventForwarder",
      "org.jfree.chart.renderer.category.AreaRenderer",
      "org.jfree.chart.annotations.XYTextAnnotation",
      "org.jfree.chart.axis.SymbolAxis",
      "com.lowagie.text.ImgCCITT",
      "com.lowagie.text.Chunk",
      "com.lowagie.text.pdf.collection.PdfCollectionSchema",
      "org.jfree.chart.axis.ValueTick",
      "com.lowagie.text.pdf.FontMapper",
      "org.jfree.chart.JFreeChart",
      "com.lowagie.text.pdf.PdfFormField",
      "com.lowagie.text.pdf.PdfDestination",
      "org.jfree.chart.renderer.xy.XYBarRenderer",
      "com.lowagie.text.pdf.PdfTable",
      "org.jfree.chart.title.LegendTitle",
      "com.lowagie.text.pdf.PdfNumber",
      "org.jfree.data.DataUtilities",
      "org.jfree.data.xy.WindDataset",
      "org.jfree.chart.ChartRenderingInfo",
      "com.lowagie.text.Chapter",
      "org.jfree.chart.renderer.xy.DeviationRenderer",
      "org.jfree.data.statistics.SimpleHistogramDataset",
      "org.jfree.chart.JFreeChartInfo",
      "org.jfree.data.time.SimpleTimePeriod",
      "com.lowagie.text.pdf.PdfPRow",
      "org.jfree.chart.event.MarkerChangeEvent",
      "org.jfree.data.time.MonthConstants",
      "com.lowagie.text.pdf.internal.PdfXConformanceImp",
      "org.jfree.data.xy.XYIntervalSeriesCollection",
      "com.lowagie.text.MarkedObject",
      "org.jfree.chart.event.AxisChangeListener",
      "com.lowagie.text.pdf.BadPdfFormatException",
      "org.jfree.chart.renderer.category.BoxAndWhiskerRenderer",
      "org.jfree.chart.renderer.xy.ClusteredXYBarRenderer",
      "org.jfree.chart.axis.SegmentedTimeline$Segment",
      "org.jfree.data.xy.DefaultXYZDataset",
      "com.lowagie.text.pdf.BaseFont",
      "com.lowagie.text.pdf.PdfAcroForm",
      "org.jfree.chart.ui.Library",
      "org.jfree.data.time.SerialDate",
      "org.jfree.chart.urls.XYZURLGenerator",
      "org.jfree.chart.block.ColumnArrangement",
      "com.lowagie.text.pdf.DefaultFontMapper",
      "org.jfree.chart.renderer.xy.StackedXYBarRenderer",
      "com.lowagie.text.pdf.PdfDocument$1",
      "org.jfree.data.general.AbstractDataset",
      "org.jfree.chart.plot.Zoomable",
      "org.jfree.chart.labels.IntervalCategoryToolTipGenerator",
      "com.lowagie.text.pdf.PdfLayerMembership",
      "com.lowagie.text.pdf.PdfEncryption",
      "com.lowagie.text.pdf.PdfGraphics2D",
      "com.lowagie.text.pdf.PRStream",
      "org.jfree.chart.util.ObjectList",
      "org.jfree.chart.renderer.category.AbstractCategoryItemRenderer",
      "com.lowagie.text.Image",
      "com.lowagie.text.pdf.PdfContentByte",
      "org.jfree.chart.plot.PiePlotState",
      "com.lowagie.text.Meta",
      "org.jfree.chart.axis.PeriodAxis",
      "org.jfree.chart.needle.ArrowNeedle",
      "com.lowagie.text.pdf.PdfIndirectObject",
      "org.jfree.chart.renderer.category.StatisticalBarRenderer",
      "org.jfree.chart.labels.StandardCategoryItemLabelGenerator",
      "com.lowagie.text.ImgTemplate",
      "org.jfree.chart.renderer.xy.YIntervalRenderer",
      "org.jfree.data.statistics.DefaultBoxAndWhiskerCategoryDataset",
      "com.lowagie.text.pdf.PdfDocument$PdfCatalog",
      "com.lowagie.text.pdf.PdfICCBased",
      "org.jfree.chart.labels.MultipleXYSeriesLabelGenerator",
      "org.jfree.data.UnknownKeyException",
      "org.jfree.chart.urls.StandardXYURLGenerator",
      "org.jfree.chart.labels.XYToolTipGenerator",
      "org.jfree.data.time.TimePeriod",
      "org.jfree.chart.renderer.WaferMapRenderer",
      "org.jfree.data.statistics.HistogramDataset",
      "org.jfree.chart.plot.MeterPlot",
      "org.jfree.chart.labels.CustomXYToolTipGenerator",
      "org.jfree.chart.util.LengthAdjustmentType",
      "org.jfree.chart.block.Block",
      "com.lowagie.text.pdf.PdfDate",
      "com.lowagie.text.ExceptionConverter",
      "com.lowagie.text.pdf.interfaces.PdfEncryptionSettings",
      "org.jfree.chart.junit.ChartPanelTests",
      "com.lowagie.text.pdf.PdfEncodings$WingdingsConversion",
      "com.lowagie.text.pdf.PdfRendition",
      "org.jfree.chart.plot.CombinedRangeXYPlot",
      "org.jfree.data.general.PieDataset",
      "org.jfree.data.general.junit.DefaultPieDatasetTests",
      "org.jfree.chart.labels.XYItemLabelGenerator",
      "org.jfree.data.KeyToGroupMap",
      "com.lowagie.text.Table",
      "org.jfree.chart.axis.AxisLocation",
      "com.lowagie.text.pdf.PdfStream",
      "com.lowagie.text.pdf.PRAcroForm$FieldInformation",
      "com.lowagie.text.SplitCharacter",
      "org.jfree.chart.event.ChartProgressEvent",
      "org.jfree.data.general.KeyedValues2DDataset",
      "com.lowagie.text.pdf.PdfTemplate",
      "org.jfree.chart.block.LengthConstraintType",
      "com.lowagie.text.pdf.PdfGraphics2D$fakeComponent",
      "com.lowagie.text.pdf.PdfObject",
      "com.lowagie.text.pdf.events.PdfPCellEventForwarder",
      "com.lowagie.text.pdf.PdfEncodings",
      "org.jfree.data.Range",
      "org.jfree.data.statistics.DefaultMultiValueCategoryDataset",
      "org.jfree.chart.labels.BoxAndWhiskerXYToolTipGenerator",
      "org.jfree.chart.plot.CombinedDomainCategoryPlot",
      "org.jfree.data.statistics.DefaultStatisticalCategoryDataset",
      "org.jfree.data.category.IntervalCategoryDataset",
      "org.jfree.chart.plot.PlotState",
      "com.lowagie.text.pdf.PdfShadingPattern",
      "org.jfree.data.time.Year",
      "org.jfree.chart.Drawable",
      "com.lowagie.text.pdf.collection.PdfCollectionSort",
      "org.jfree.chart.axis.CategoryLabelPosition",
      "org.jfree.chart.renderer.xy.XYAreaRenderer$XYAreaRendererState",
      "com.lowagie.text.pdf.BidiLine",
      "org.jfree.chart.imagemap.ToolTipTagFragmentGenerator",
      "org.jfree.chart.text.TextLine",
      "org.jfree.data.time.TimePeriodValuesCollection",
      "org.jfree.data.xy.XIntervalDataItem",
      "com.lowagie.text.pdf.PdfSigGenericPKCS$PPKMS",
      "com.lowagie.text.pdf.interfaces.PdfAnnotations",
      "com.lowagie.text.pdf.MappedRandomAccessFile",
      "org.jfree.chart.PaintMap",
      "com.lowagie.text.pdf.interfaces.PdfRunDirection",
      "com.lowagie.text.pdf.PdfContentByte$GraphicState",
      "org.jfree.chart.renderer.xy.XYBubbleRenderer",
      "org.jfree.chart.axis.DateTickUnit",
      "org.jfree.chart.annotations.XYBoxAnnotation",
      "org.jfree.chart.axis.ModuloAxis",
      "com.lowagie.text.pdf.PdfFileSpecification",
      "com.lowagie.text.pdf.PdfDocument$Indentation",
      "org.jfree.chart.needle.PinNeedle",
      "org.jfree.data.xy.TableXYDataset",
      "org.jfree.chart.entity.ChartEntity",
      "org.jfree.chart.entity.EntityCollection",
      "com.lowagie.text.Rectangle",
      "org.jfree.data.gantt.GanttCategoryDataset",
      "org.jfree.data.jdbc.JDBCCategoryDataset",
      "org.jfree.data.Values2D",
      "com.lowagie.text.pdf.HyphenationEvent",
      "org.jfree.chart.renderer.GrayPaintScale",
      "org.jfree.data.statistics.BoxAndWhiskerXYDataset",
      "org.jfree.data.general.DatasetChangeEvent",
      "com.lowagie.text.pdf.interfaces.PdfDocumentActions",
      "org.jfree.data.general.SeriesChangeListener",
      "org.jfree.data.category.DefaultCategoryDataset",
      "org.jfree.chart.axis.TickType",
      "org.jfree.chart.renderer.xy.DefaultXYItemRenderer",
      "com.lowagie.text.pdf.PdfColor",
      "org.jfree.data.general.KeyedValuesDataset",
      "org.jfree.data.time.Week",
      "org.jfree.chart.block.GridArrangement",
      "org.jfree.chart.entity.PieSectionEntity",
      "org.jfree.data.xy.VectorSeries",
      "org.jfree.data.general.DefaultKeyedValuesDataset",
      "org.jfree.chart.util.Layer",
      "com.lowagie.text.pdf.IntHashtable",
      "org.jfree.chart.event.PlotChangeListener",
      "org.jfree.data.general.Series",
      "com.lowagie.text.pdf.Type1Font",
      "com.lowagie.text.BadElementException",
      "com.lowagie.text.pdf.PdfString",
      "com.lowagie.text.pdf.PdfContents",
      "org.jfree.data.xy.XYSeriesCollection",
      "org.jfree.chart.plot.DrawingSupplier",
      "com.lowagie.text.pdf.PdfPageLabels$PdfPageLabelFormat",
      "org.jfree.chart.renderer.PaintScale",
      "org.jfree.chart.axis.StandardTickUnitSource",
      "org.jfree.data.KeyedValue",
      "com.lowagie.text.pdf.interfaces.PdfXConformance",
      "org.jfree.chart.urls.StandardCategoryURLGenerator",
      "org.jfree.chart.labels.HighLowItemLabelGenerator",
      "org.jfree.chart.plot.IntervalMarker",
      "org.jfree.chart.util.PublicCloneable",
      "com.lowagie.text.pdf.BaseFont$StreamFont",
      "org.jfree.data.xy.IntervalXYDelegate",
      "com.lowagie.text.Document",
      "com.lowagie.text.pdf.PdfFont",
      "org.jfree.chart.StrokeMap",
      "com.lowagie.text.pdf.PdfResources",
      "org.jfree.data.KeyedObjects2D",
      "org.jfree.chart.annotations.CategoryLineAnnotation",
      "com.lowagie.text.pdf.OutputStreamEncryption",
      "org.jfree.chart.axis.LogAxis",
      "org.jfree.chart.axis.CategoryLabelWidthType"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.jfree.chart.block.BlockContainer", false, BorderArrangement_ESTest_scaffolding.class.getClassLoader()));
  }
}
