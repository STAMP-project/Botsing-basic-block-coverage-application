/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:02:02 UTC 2020
 */

package org.jfree.chart.plot;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.awt.Graphics2D;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.event.AxisChangeEvent;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.xy.DefaultTableXYDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      XYPlot xYPlot0 = new XYPlot();
      xYPlot0.getBackgroundImageAlignment();
      ValueAxis valueAxis0 = mock(ValueAxis.class, new ViolatedAssumptionAnswer());
      xYPlot0.getDataRange((ValueAxis) null);
      xYPlot0.getDrawingSupplier();
      Graphics2D graphics2D0 = mock(Graphics2D.class, new ViolatedAssumptionAnswer());
      xYPlot0.getDomainZeroBaselineStroke();
      AxisChangeEvent axisChangeEvent0 = mock(AxisChangeEvent.class, new ViolatedAssumptionAnswer());
      DefaultTableXYDataset defaultTableXYDataset0 = new DefaultTableXYDataset();
      XYPlot xYPlot1 = new XYPlot(defaultTableXYDataset0, (ValueAxis) null, (ValueAxis) null, (XYItemRenderer) null);
      xYPlot1.getBackgroundImageAlignment();
      xYPlot1.getDataRange(valueAxis0);
      ValueAxis valueAxis1 = null;
      NumberAxis numberAxis0 = new NumberAxis();
      xYPlot0.getDataRange(numberAxis0);
      JFreeChart jFreeChart0 = new JFreeChart(xYPlot1);
      XYPlot xYPlot2 = jFreeChart0.getXYPlot();
      // Undeclared exception!
      xYPlot2.getDataRange((ValueAxis) null);
  }
}
