/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:57:51 UTC 2020
 */

package org.jfree.chart.plot;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.awt.Stroke;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.jfree.chart.axis.CyclicNumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.data.time.TimeSeriesCollection;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      TimeSeriesCollection timeSeriesCollection0 = new TimeSeriesCollection();
      ValueAxis valueAxis0 = mock(ValueAxis.class, new ViolatedAssumptionAnswer());
      XYPlot xYPlot0 = new XYPlot(timeSeriesCollection0, valueAxis0, valueAxis0, (XYItemRenderer) null);
      xYPlot0.getNoDataMessagePaint();
      xYPlot0.getAxisOffset();
      Stroke stroke0 = xYPlot0.getDomainZeroBaselineStroke();
      xYPlot0.setDomainGridlineStroke(stroke0);
      CyclicNumberAxis cyclicNumberAxis0 = new CyclicNumberAxis((-1690.4656), "");
      XYLineAndShapeRenderer xYLineAndShapeRenderer0 = new XYLineAndShapeRenderer();
      XYPlot xYPlot1 = new XYPlot(timeSeriesCollection0, cyclicNumberAxis0, cyclicNumberAxis0, (XYItemRenderer) null);
  }
}
