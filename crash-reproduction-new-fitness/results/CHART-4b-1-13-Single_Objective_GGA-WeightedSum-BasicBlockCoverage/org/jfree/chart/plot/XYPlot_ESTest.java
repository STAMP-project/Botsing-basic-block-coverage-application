/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 12:36:26 UTC 2020
 */

package org.jfree.chart.plot;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.xy.CategoryTableXYDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      CategoryTableXYDataset categoryTableXYDataset0 = new CategoryTableXYDataset();
      ValueAxis valueAxis0 = null;
      XYPlot xYPlot0 = new XYPlot(categoryTableXYDataset0, (ValueAxis) null, (ValueAxis) null, (XYItemRenderer) null);
      boolean boolean0 = true;
      xYPlot0.setDomainMinorGridlinesVisible(true);
      // Undeclared exception!
      xYPlot0.getDataRange((ValueAxis) null);
  }
}
