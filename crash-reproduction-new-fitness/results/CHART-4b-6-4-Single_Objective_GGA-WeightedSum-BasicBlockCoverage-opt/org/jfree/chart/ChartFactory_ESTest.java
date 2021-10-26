/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 15:18:28 UTC 2021
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
import org.jfree.data.general.PieDataset;
import org.jfree.data.xy.XYDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class ChartFactory_ESTest extends ChartFactory_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ChartFactory.getChartTheme();
      CategoryDataset categoryDataset0 = mock(CategoryDataset.class, new ViolatedAssumptionAnswer());
      doReturn(0, 0, 0, 0).when(categoryDataset0).getColumnCount();
      doReturn(0, 0, 0, 0).when(categoryDataset0).getRowCount();
      PlotOrientation plotOrientation0 = mock(PlotOrientation.class, new ViolatedAssumptionAnswer());
      ChartFactory.createAreaChart("", "", "", categoryDataset0, plotOrientation0, true, true, false);
      PieDataset pieDataset0 = mock(PieDataset.class, new ViolatedAssumptionAnswer());
      XYDataset xYDataset0 = mock(XYDataset.class, new ViolatedAssumptionAnswer());
      doReturn(0).when(xYDataset0).getSeriesCount();
      PlotOrientation plotOrientation1 = mock(PlotOrientation.class, new ViolatedAssumptionAnswer());
      boolean boolean0 = false;
      // Undeclared exception!
      ChartFactory.createScatterPlot("b*naAx%cbX Y@S[/", "gNb5jcP6!A9G+N%)|", "gNb5jcP6!A9G+N%)|", xYDataset0, plotOrientation1, false, false, false);
  }
}
