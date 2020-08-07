/*
 * This file was automatically generated by EvoSuite
 * Mon Mar 30 16:24:22 UTC 2020
 */

package org.jfree.chart;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.axis.SegmentedTimeline;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesCollection;
import org.jfree.data.xy.XYBarDataset;
import org.junit.runner.RunWith;
import sun.util.calendar.ZoneInfo;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class ChartFactory_ESTest extends ChartFactory_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      TimeSeries timeSeries0 = new TimeSeries("", " (+100%)", "Null 'orientation' argument.");
      ZoneInfo zoneInfo0 = (ZoneInfo)SegmentedTimeline.DEFAULT_TIME_ZONE;
      TimeSeriesCollection timeSeriesCollection0 = new TimeSeriesCollection(timeSeries0, zoneInfo0);
      XYBarDataset xYBarDataset0 = new XYBarDataset(timeSeriesCollection0, (-652.91126242496));
      PlotOrientation plotOrientation0 = mock(PlotOrientation.class, new ViolatedAssumptionAnswer());
      // Undeclared exception!
      ChartFactory.createScatterPlot(" (+100%)", (String) null, "", xYBarDataset0, plotOrientation0, true, true, true);
  }
}
