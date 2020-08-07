/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 12:29:52 UTC 2020
 */

package org.jfree.chart.plot;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.jfree.chart.axis.CyclicNumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.xy.VectorSeriesCollection;
import org.jfree.data.xy.XYDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      XYDataset xYDataset0 = mock(XYDataset.class, new ViolatedAssumptionAnswer());
      doReturn((String) null, (String) null).when(xYDataset0).toString();
      ValueAxis valueAxis0 = mock(ValueAxis.class, new ViolatedAssumptionAnswer());
      XYPlot xYPlot0 = new XYPlot(xYDataset0, valueAxis0, valueAxis0, (XYItemRenderer) null);
      VectorSeriesCollection vectorSeriesCollection0 = new VectorSeriesCollection();
      CyclicNumberAxis cyclicNumberAxis0 = new CyclicNumberAxis(0.0, ">~h<8X@_UN:7C");
      XYPlot xYPlot1 = new XYPlot(vectorSeriesCollection0, cyclicNumberAxis0, cyclicNumberAxis0, (XYItemRenderer) null);
  }
}
