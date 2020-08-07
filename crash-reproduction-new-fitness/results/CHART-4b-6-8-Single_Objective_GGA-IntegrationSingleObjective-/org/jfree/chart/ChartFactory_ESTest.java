/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:36:28 UTC 2020
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
import org.jfree.data.category.CategoryDataset;
import org.jfree.data.xy.IntervalXYDataset;
import org.jfree.data.xy.XYDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class ChartFactory_ESTest extends ChartFactory_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ChartFactory.getChartTheme();
      IntervalXYDataset intervalXYDataset0 = mock(IntervalXYDataset.class, new ViolatedAssumptionAnswer());
      doReturn((String) null, (String) null, (String) null, (String) null, (String) null).when(intervalXYDataset0).toString();
      doReturn(0, 0, 0, 0, 0).when(intervalXYDataset0).getSeriesCount();
      PlotOrientation plotOrientation0 = mock(PlotOrientation.class, new ViolatedAssumptionAnswer());
      ChartFactory.createHistogram("", "", "", intervalXYDataset0, plotOrientation0, true, true, true);
      CategoryDataset categoryDataset0 = mock(CategoryDataset.class, new ViolatedAssumptionAnswer());
      doReturn(0, 0).when(categoryDataset0).getColumnCount();
      doReturn(0, 0).when(categoryDataset0).getRowCount();
      PlotOrientation plotOrientation1 = mock(PlotOrientation.class, new ViolatedAssumptionAnswer());
      boolean boolean0 = true;
      ChartFactory.createAreaChart("", "The 'alpha' value must be in the range 0.0f to 1.0f", "", categoryDataset0, plotOrientation1, true, false, true);
      XYDataset xYDataset0 = mock(XYDataset.class, new ViolatedAssumptionAnswer());
      doReturn((String) null, (String) null).when(xYDataset0).toString();
      doReturn(0).when(xYDataset0).getSeriesCount();
      PlotOrientation plotOrientation2 = mock(PlotOrientation.class, new ViolatedAssumptionAnswer());
      // Undeclared exception!
      ChartFactory.createScatterPlot("", "", "0x6681CC", xYDataset0, plotOrientation2, false, false, false);
  }
}
