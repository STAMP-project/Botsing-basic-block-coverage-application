/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:34:21 UTC 2020
 */

package org.jfree.chart;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.xy.WindDataset;
import org.jfree.data.xy.XYDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class ChartFactory_ESTest extends ChartFactory_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      WindDataset windDataset0 = mock(WindDataset.class, new ViolatedAssumptionAnswer());
      doReturn((String) null, (String) null, (String) null, (String) null, (String) null).when(windDataset0).toString();
      doReturn(0, 0, 0).when(windDataset0).getSeriesCount();
      ChartFactory.createWindPlot("!:bg'5GCVg:F{Btul|", "", "YgzPWETkL}j", windDataset0, false, true, false);
      XYDataset xYDataset0 = mock(XYDataset.class, new ViolatedAssumptionAnswer());
      doReturn((String) null, (String) null).when(xYDataset0).toString();
      doReturn(0).when(xYDataset0).getSeriesCount();
      PlotOrientation plotOrientation0 = mock(PlotOrientation.class, new ViolatedAssumptionAnswer());
      // Undeclared exception!
      ChartFactory.createScatterPlot("vV4/.", "", "RectangleAnchor.TOP_LEFT", xYDataset0, plotOrientation0, false, false, true);
  }
}
