/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:32:30 UTC 2020
 */

package org.jfree.chart.plot;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.jfree.chart.axis.CyclicNumberAxis;
import org.jfree.chart.axis.ExtendedCategoryAxis;
import org.jfree.chart.axis.LogAxis;
import org.jfree.chart.axis.NumberAxis3D;
import org.jfree.chart.plot.MultiplePiePlot;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.VectorRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.xy.DefaultXYDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MultiplePiePlot multiplePiePlot0 = new MultiplePiePlot();
      DefaultXYDataset defaultXYDataset0 = new DefaultXYDataset();
      CyclicNumberAxis cyclicNumberAxis0 = new CyclicNumberAxis(711, 972.0, "[glfsq9JpHz");
      VectorRenderer vectorRenderer0 = new VectorRenderer();
      XYPlot xYPlot0 = new XYPlot(defaultXYDataset0, cyclicNumberAxis0, cyclicNumberAxis0, vectorRenderer0);
      ExtendedCategoryAxis extendedCategoryAxis0 = new ExtendedCategoryAxis("HYd~C");
      extendedCategoryAxis0.getTickMarkPaint();
      cyclicNumberAxis0.setMinorTickMarksVisible(true);
      NumberAxis3D numberAxis3D0 = new NumberAxis3D("");
      LogAxis logAxis0 = new LogAxis("");
      XYPlot xYPlot1 = new XYPlot(defaultXYDataset0, numberAxis3D0, logAxis0, (XYItemRenderer) null);
  }
}
