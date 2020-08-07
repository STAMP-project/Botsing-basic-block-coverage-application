/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:31:21 UTC 2020
 */

package org.jfree.chart.plot;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.jfree.chart.axis.NumberAxis3D;
import org.jfree.chart.axis.SegmentedTimeline;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYBlockRenderer;
import org.jfree.chart.renderer.xy.XYBoxAndWhiskerRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.general.SeriesChangeEvent;
import org.jfree.data.general.SeriesChangeInfo;
import org.jfree.data.general.SeriesChangeType;
import org.jfree.data.time.Day;
import org.jfree.data.time.DynamicTimeSeriesCollection;
import org.junit.runner.RunWith;
import sun.util.calendar.ZoneInfo;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PlotOrientation plotOrientation0 = mock(PlotOrientation.class, new ViolatedAssumptionAnswer());
      Day day0 = new Day();
      ZoneInfo zoneInfo0 = (ZoneInfo)SegmentedTimeline.DEFAULT_TIME_ZONE;
      DynamicTimeSeriesCollection dynamicTimeSeriesCollection0 = new DynamicTimeSeriesCollection(145, 145, day0, zoneInfo0);
      ValueAxis valueAxis0 = null;
      SeriesChangeType seriesChangeType0 = SeriesChangeType.UPDATE;
      SeriesChangeInfo seriesChangeInfo0 = new SeriesChangeInfo(seriesChangeType0, 24, 1685348972);
      SeriesChangeEvent seriesChangeEvent0 = new SeriesChangeEvent(dynamicTimeSeriesCollection0, seriesChangeInfo0);
      dynamicTimeSeriesCollection0.seriesChanged(seriesChangeEvent0);
      XYBlockRenderer xYBlockRenderer0 = new XYBlockRenderer();
      XYPlot xYPlot0 = new XYPlot(dynamicTimeSeriesCollection0, (ValueAxis) null, (ValueAxis) null, xYBlockRenderer0);
      xYPlot0.getNoDataMessagePaint();
      xYPlot0.getDomainAxisEdge(145);
      xYPlot0.isRangePannable();
      xYPlot0.getIndexOf(xYBlockRenderer0);
      xYPlot0.getBackgroundAlpha();
      xYPlot0.isRangeZoomable();
      xYPlot0.getDataRange((ValueAxis) null);
      xYPlot0.getDatasetCount();
      NumberAxis3D numberAxis3D0 = new NumberAxis3D("");
      XYBoxAndWhiskerRenderer xYBoxAndWhiskerRenderer0 = new XYBoxAndWhiskerRenderer();
      XYPlot xYPlot1 = new XYPlot(dynamicTimeSeriesCollection0, (ValueAxis) null, numberAxis3D0, (XYItemRenderer) null);
  }
}
