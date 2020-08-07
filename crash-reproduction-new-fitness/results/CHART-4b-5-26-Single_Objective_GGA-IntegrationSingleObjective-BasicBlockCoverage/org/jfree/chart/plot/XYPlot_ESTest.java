/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:02:07 UTC 2020
 */

package org.jfree.chart.plot;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.CyclicNumberAxis;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.axis.PeriodAxis;
import org.jfree.chart.plot.Plot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.PolarPlot;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.DefaultPolarItemRenderer;
import org.jfree.chart.renderer.PolarItemRenderer;
import org.jfree.chart.renderer.xy.StandardXYItemRenderer;
import org.jfree.chart.renderer.xy.XYDifferenceRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.gantt.TaskSeriesCollection;
import org.jfree.data.gantt.XYTaskDataset;
import org.jfree.data.xy.MatrixSeriesCollection;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
    Future<?> future = executor.submit(new Runnable(){ 
            @Override public void run() { 
          TaskSeriesCollection taskSeriesCollection0 = new TaskSeriesCollection();
          XYTaskDataset xYTaskDataset0 = new XYTaskDataset(taskSeriesCollection0);
          double double0 = 0.0;
          String string0 = "^.MA'j";
          CyclicNumberAxis cyclicNumberAxis0 = new CyclicNumberAxis(4218.3234916, 0.0, "^.MA'j");
          StandardXYItemRenderer standardXYItemRenderer0 = new StandardXYItemRenderer();
          XYPlot xYPlot0 = new XYPlot(xYTaskDataset0, cyclicNumberAxis0, cyclicNumberAxis0, standardXYItemRenderer0);
          int int0 = (-2);
          DefaultPolarItemRenderer defaultPolarItemRenderer0 = new DefaultPolarItemRenderer();
          MatrixSeriesCollection matrixSeriesCollection0 = new MatrixSeriesCollection();
          PolarPlot polarPlot0 = new PolarPlot(matrixSeriesCollection0, cyclicNumberAxis0, (PolarItemRenderer) null);
          PlotOrientation plotOrientation0 = polarPlot0.getOrientation();
          AxisLocation axisLocation0 = xYPlot0.getDomainAxisLocation(1);
          JFreeChart jFreeChart0 = new JFreeChart(polarPlot0);
          ChartPanel chartPanel0 = new ChartPanel(jFreeChart0, false);
          PlotOrientation plotOrientation1 = chartPanel0.getOrientation();
          Plot.resolveRangeAxisLocation(axisLocation0, plotOrientation1);
          XYDifferenceRenderer xYDifferenceRenderer0 = new XYDifferenceRenderer();
          XYPlot xYPlot1 = new XYPlot();
          xYPlot1.isRangeZeroBaselineVisible();
          int int1 = 10215;
          int int2 = 0;
          xYPlot0.setWeight(0);
          xYPlot1.setRangeGridlinesVisible(false);
          xYPlot0.getRangeCrosshairValue();
          PeriodAxis periodAxis0 = new PeriodAxis("^.MA'j");
          Plot.resolveRangeAxisLocation(axisLocation0, plotOrientation0);
          Plot.resolveRangeAxisLocation(axisLocation0, plotOrientation0);
          DateAxis dateAxis0 = new DateAxis();
          XYPlot xYPlot2 = new XYPlot(xYTaskDataset0, periodAxis0, dateAxis0, (XYItemRenderer) null);
      } 
    });
    future.get(4000, TimeUnit.MILLISECONDS);
  }
}
