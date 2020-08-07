/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:35:03 UTC 2020
 */

package org.jfree.chart.plot;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.sql.Connection;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.PiePlot;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.SamplingXYLineRenderer;
import org.jfree.chart.renderer.xy.StackedXYBarRenderer;
import org.jfree.chart.renderer.xy.XYAreaRenderer;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.jdbc.JDBCXYDataset;
import org.jfree.data.xy.DefaultIntervalXYDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DefaultIntervalXYDataset defaultIntervalXYDataset0 = new DefaultIntervalXYDataset();
      NumberAxis numberAxis0 = new NumberAxis("");
      Connection connection0 = mock(Connection.class, new ViolatedAssumptionAnswer());
      JDBCXYDataset jDBCXYDataset0 = new JDBCXYDataset(connection0);
      XYAreaRenderer xYAreaRenderer0 = new XYAreaRenderer();
      XYPlot xYPlot0 = new XYPlot(jDBCXYDataset0, numberAxis0, numberAxis0, xYAreaRenderer0);
      PiePlot piePlot0 = new PiePlot();
      XYBarRenderer xYBarRenderer0 = new XYBarRenderer();
      StackedXYBarRenderer stackedXYBarRenderer0 = new StackedXYBarRenderer();
      SamplingXYLineRenderer samplingXYLineRenderer0 = new SamplingXYLineRenderer();
      XYPlot xYPlot1 = new XYPlot(jDBCXYDataset0, numberAxis0, numberAxis0, (XYItemRenderer) null);
  }
}
