/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 12:37:55 UTC 2020
 */

package org.jfree.chart.plot;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.DrawingSupplier;
import org.jfree.chart.plot.Plot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.xy.DefaultTableXYDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      AxisLocation axisLocation0 = AxisLocation.TOP_OR_RIGHT;
      PlotOrientation plotOrientation0 = PlotOrientation.VERTICAL;
      Plot.resolveDomainAxisLocation(axisLocation0, plotOrientation0);
      XYPlot xYPlot0 = new XYPlot();
      ValueAxis valueAxis0 = mock(ValueAxis.class, new ViolatedAssumptionAnswer());
      xYPlot0.getDataRange(valueAxis0);
      PlotOrientation plotOrientation1 = mock(PlotOrientation.class, new ViolatedAssumptionAnswer());
      xYPlot0.setOrientation(plotOrientation1);
      xYPlot0.setRangeAxisLocation(axisLocation0, true);
      JFreeChart jFreeChart0 = new JFreeChart(xYPlot0);
      XYPlot xYPlot1 = jFreeChart0.getXYPlot();
      DrawingSupplier drawingSupplier0 = xYPlot1.getDrawingSupplier();
      xYPlot0.setDrawingSupplier(drawingSupplier0);
      DefaultTableXYDataset defaultTableXYDataset0 = new DefaultTableXYDataset(true);
      NumberAxis numberAxis0 = new NumberAxis("");
      XYPlot xYPlot2 = new XYPlot(defaultTableXYDataset0, numberAxis0, numberAxis0, (XYItemRenderer) null);
  }
}
