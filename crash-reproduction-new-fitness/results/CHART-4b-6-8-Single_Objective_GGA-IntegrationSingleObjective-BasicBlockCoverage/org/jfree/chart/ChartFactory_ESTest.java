/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:35:00 UTC 2020
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
import org.jfree.data.xy.IntervalXYDataset;
import org.jfree.data.xy.XYDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class ChartFactory_ESTest extends ChartFactory_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      IntervalXYDataset intervalXYDataset0 = mock(IntervalXYDataset.class, new ViolatedAssumptionAnswer());
      doReturn((String) null, (String) null, (String) null, (String) null, (String) null).when(intervalXYDataset0).toString();
      doReturn(0, 0, 0, 0, 0).when(intervalXYDataset0).getSeriesCount();
      PlotOrientation plotOrientation0 = mock(PlotOrientation.class, new ViolatedAssumptionAnswer());
      boolean boolean0 = true;
      ChartFactory.createHistogram(" tcH4dF:n/", " tcH4dF:n/", " tcH4dF:n/", intervalXYDataset0, plotOrientation0, true, false, true);
      XYDataset xYDataset0 = mock(XYDataset.class, new ViolatedAssumptionAnswer());
      doReturn((String) null, (String) null).when(xYDataset0).toString();
      doReturn(0).when(xYDataset0).getSeriesCount();
      PlotOrientation plotOrientation1 = mock(PlotOrientation.class, new ViolatedAssumptionAnswer());
      boolean boolean1 = false;
      // Undeclared exception!
      ChartFactory.createScatterPlot(" tcH4dF:n/", ".t(%54+Zc,+E\"", " tcH4dF:n/", xYDataset0, plotOrientation1, false, true, false);
  }
}
