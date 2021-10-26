/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 15:19:34 UTC 2021
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
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.chart.renderer.xy.XYStepAreaRenderer;
import org.jfree.data.xy.YIntervalSeriesCollection;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      AxisLocation axisLocation0 = AxisLocation.BOTTOM_OR_RIGHT;
      PlotOrientation plotOrientation0 = mock(PlotOrientation.class, new ViolatedAssumptionAnswer());
      YIntervalSeriesCollection yIntervalSeriesCollection0 = new YIntervalSeriesCollection();
      NumberAxis3D numberAxis3D0 = new NumberAxis3D("Requires 'index' >= 0.");
      numberAxis3D0.centerRange(625.7116);
      XYStepAreaRenderer xYStepAreaRenderer0 = new XYStepAreaRenderer((-3201));
      XYPlot xYPlot0 = new XYPlot(yIntervalSeriesCollection0, numberAxis3D0, numberAxis3D0, (XYItemRenderer) null);
      // Undeclared exception!
      xYPlot0.getDataRange(numberAxis3D0);
  }
}
