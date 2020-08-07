/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:57:07 UTC 2020
 */

package org.jfree.chart.plot;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.NumberAxis3D;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.labels.StandardXYToolTipGenerator;
import org.jfree.chart.plot.CombinedRangeCategoryPlot;
import org.jfree.chart.plot.Plot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.PolarPlot;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.SamplingXYLineRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.xy.XYDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      XYDataset xYDataset0 = mock(XYDataset.class, new ViolatedAssumptionAnswer());
      doReturn((String) null, (String) null).when(xYDataset0).toString();
      doReturn(0).when(xYDataset0).getSeriesCount();
      ValueAxis valueAxis0 = mock(ValueAxis.class, new ViolatedAssumptionAnswer());
      StandardXYToolTipGenerator standardXYToolTipGenerator0 = new StandardXYToolTipGenerator();
      PolarPlot polarPlot0 = new PolarPlot();
      PlotOrientation plotOrientation0 = polarPlot0.getOrientation();
      CombinedRangeCategoryPlot combinedRangeCategoryPlot0 = new CombinedRangeCategoryPlot(valueAxis0);
      AxisLocation axisLocation0 = combinedRangeCategoryPlot0.getRangeAxisLocation();
      Plot.resolveDomainAxisLocation(axisLocation0, plotOrientation0);
      SamplingXYLineRenderer samplingXYLineRenderer0 = new SamplingXYLineRenderer();
      NumberAxis3D numberAxis3D0 = new NumberAxis3D("Null 'state' argument.");
      XYPlot xYPlot0 = new XYPlot(xYDataset0, numberAxis3D0, numberAxis3D0, (XYItemRenderer) null);
  }
}
