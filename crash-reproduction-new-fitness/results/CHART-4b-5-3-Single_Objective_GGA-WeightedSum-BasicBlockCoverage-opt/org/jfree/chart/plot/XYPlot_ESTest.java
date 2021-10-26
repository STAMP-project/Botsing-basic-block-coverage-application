/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 15:18:27 UTC 2021
 */

package org.jfree.chart.plot;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.jfree.chart.axis.CyclicNumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.ThermometerPlot;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.xy.DefaultXYZDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DefaultXYZDataset defaultXYZDataset0 = new DefaultXYZDataset();
      CategoryPlot categoryPlot0 = new CategoryPlot();
      CyclicNumberAxis cyclicNumberAxis0 = new CyclicNumberAxis((-1.0));
      ThermometerPlot thermometerPlot0 = new ThermometerPlot();
      ValueAxis valueAxis0 = thermometerPlot0.getRangeAxis();
      XYPlot xYPlot0 = new XYPlot(defaultXYZDataset0, cyclicNumberAxis0, valueAxis0, (XYItemRenderer) null);
  }
}
