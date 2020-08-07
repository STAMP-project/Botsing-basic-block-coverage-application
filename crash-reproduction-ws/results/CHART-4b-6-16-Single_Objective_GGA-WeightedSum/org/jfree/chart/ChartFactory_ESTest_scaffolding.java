/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon Mar 30 16:32:16 UTC 2020
 */

package org.jfree.chart;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class ChartFactory_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "org.jfree.chart.ChartFactory"; 
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
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(ChartFactory_ESTest_scaffolding.class.getClassLoader() ,
      "org.jfree.chart.entity.AxisEntity",
      "org.jfree.chart.ChartTheme",
      "org.jfree.chart.plot.ValueMarker",
      "org.jfree.data.DomainOrder",
      "org.jfree.chart.plot.WaferMapPlot",
      "org.jfree.chart.renderer.AbstractRenderer",
      "org.jfree.chart.labels.CategorySeriesLabelGenerator",
      "org.jfree.chart.Effect3D",
      "org.jfree.chart.plot.RingPlot",
      "org.jfree.data.general.AbstractDataset",
      "org.jfree.chart.plot.Zoomable",
      "org.jfree.chart.title.Title",
      "org.jfree.chart.labels.PieToolTipGenerator",
      "org.jfree.chart.renderer.xy.XYItemRenderer",
      "org.jfree.chart.axis.CategoryAxis",
      "org.jfree.chart.util.ObjectList",
      "org.jfree.chart.renderer.category.AbstractCategoryItemRenderer",
      "org.jfree.chart.renderer.xy.GradientXYBarPainter",
      "org.jfree.chart.renderer.category.BarPainter",
      "org.jfree.chart.axis.DateAxis",
      "org.jfree.data.general.SeriesDataset",
      "org.jfree.chart.plot.CategoryPlot",
      "org.jfree.chart.StandardChartTheme",
      "org.jfree.chart.event.MarkerChangeListener",
      "org.jfree.data.xy.XYDataset",
      "org.jfree.chart.plot.Plot",
      "org.jfree.chart.event.PlotChangeEvent",
      "org.jfree.chart.plot.ValueAxisPlot",
      "org.jfree.chart.plot.PiePlot",
      "org.jfree.chart.plot.PiePlot3D",
      "org.jfree.chart.labels.XYToolTipGenerator",
      "org.jfree.chart.renderer.category.BarRenderer",
      "org.jfree.chart.event.AxisChangeEvent",
      "org.jfree.chart.axis.Timeline",
      "org.jfree.chart.urls.CategoryURLGenerator",
      "org.jfree.chart.renderer.xy.XYBarPainter",
      "org.jfree.chart.labels.PieSectionLabelGenerator",
      "org.jfree.chart.block.Block",
      "org.jfree.chart.axis.Axis",
      "org.jfree.chart.urls.PieURLGenerator",
      "org.jfree.chart.axis.NumberAxis3D",
      "org.jfree.data.general.PieDataset",
      "org.jfree.chart.entity.CategoryItemEntity",
      "org.jfree.data.statistics.BoxAndWhiskerCategoryDataset",
      "org.jfree.chart.entity.AxisLabelEntity",
      "org.jfree.data.general.DatasetUtilities",
      "org.jfree.chart.util.UnitType",
      "org.jfree.chart.urls.XYURLGenerator",
      "org.jfree.chart.util.AbstractObjectList",
      "org.jfree.data.xy.IntervalXYDataset",
      "org.jfree.chart.plot.Selectable",
      "org.jfree.chart.plot.DefaultDrawingSupplier",
      "org.jfree.data.category.CategoryDataset",
      "org.jfree.data.Range",
      "org.jfree.chart.plot.DatasetRenderingOrder",
      "org.jfree.chart.event.RendererChangeListener",
      "org.jfree.data.KeyedValues",
      "org.jfree.chart.util.RectangleInsets",
      "org.jfree.chart.labels.CategoryToolTipGenerator",
      "org.jfree.chart.renderer.category.GradientBarPainter",
      "org.jfree.chart.event.TitleChangeListener",
      "org.jfree.data.category.IntervalCategoryDataset",
      "org.jfree.chart.text.TextMeasurer",
      "org.jfree.chart.axis.TickUnit",
      "org.jfree.chart.LegendItemSource",
      "org.jfree.chart.ChartColor",
      "org.jfree.chart.Drawable",
      "org.jfree.chart.util.GradientPaintTransformer",
      "org.jfree.chart.renderer.PolarItemRenderer",
      "org.jfree.data.xy.XYZDataset",
      "org.jfree.data.DomainInfo",
      "org.jfree.chart.plot.PieLabelLinkStyle",
      "org.jfree.data.general.DatasetChangeListener",
      "org.jfree.chart.renderer.category.BarRenderer3D",
      "org.jfree.chart.plot.PolarPlot",
      "org.jfree.chart.axis.CategoryAxis3D",
      "org.jfree.chart.axis.TickUnitSource",
      "org.jfree.data.general.DatasetGroup",
      "org.jfree.chart.plot.XYPlot",
      "org.jfree.chart.block.AbstractBlock",
      "org.jfree.chart.event.ChartChangeEventType",
      "org.jfree.data.xy.OHLCDataset",
      "org.jfree.data.xy.TableXYDataset",
      "org.jfree.chart.util.TableOrder",
      "org.jfree.chart.ChartFactory",
      "org.jfree.chart.renderer.category.CategoryItemRenderer",
      "org.jfree.chart.entity.ChartEntity",
      "org.jfree.chart.plot.Pannable",
      "org.jfree.data.Values2D",
      "org.jfree.data.RangeType",
      "org.jfree.data.statistics.BoxAndWhiskerXYDataset",
      "org.jfree.data.general.DatasetChangeEvent",
      "org.jfree.chart.axis.TickUnits",
      "org.jfree.data.general.WaferMapDataset",
      "org.jfree.data.general.Dataset",
      "org.jfree.chart.plot.SeriesRenderingOrder",
      "org.jfree.chart.plot.PlotOrientation",
      "org.jfree.chart.axis.NumberAxis",
      "org.jfree.chart.axis.SymbolAxis",
      "org.jfree.chart.event.PlotChangeListener",
      "org.jfree.chart.title.PaintScaleLegend",
      "org.jfree.chart.axis.NumberTickUnit",
      "org.jfree.chart.JFreeChart",
      "org.jfree.chart.plot.Marker",
      "org.jfree.chart.title.TextTitle",
      "org.jfree.chart.plot.DrawingSupplier",
      "org.jfree.chart.title.LegendTitle",
      "org.jfree.chart.axis.ValueAxis",
      "org.jfree.data.xy.WindDataset",
      "org.jfree.chart.util.PublicCloneable",
      "org.jfree.chart.event.ChartChangeEvent",
      "org.jfree.chart.plot.MultiplePiePlot",
      "org.jfree.data.KeyedValues2D",
      "org.jfree.data.Values",
      "org.jfree.chart.event.AxisChangeListener",
      "org.jfree.chart.util.ResourceBundleWrapper",
      "org.jfree.chart.entity.PlotEntity"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.jfree.chart.plot.PlotOrientation", false, ChartFactory_ESTest_scaffolding.class.getClassLoader()));
    mock(Class.forName("org.jfree.data.xy.XYDataset", false, ChartFactory_ESTest_scaffolding.class.getClassLoader()));
  }
}
