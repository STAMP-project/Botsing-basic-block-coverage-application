/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:59:32 UTC 2020
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
import org.jfree.chart.ChartTheme;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.category.IntervalCategoryDataset;
import org.jfree.data.statistics.BoxAndWhiskerXYDataset;
import org.jfree.data.xy.XYDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class ChartFactory_ESTest extends ChartFactory_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ChartTheme chartTheme0 = mock(ChartTheme.class, new ViolatedAssumptionAnswer());
      ChartFactory.setChartTheme(chartTheme0);
      String string0 = "";
      IntervalCategoryDataset intervalCategoryDataset0 = mock(IntervalCategoryDataset.class, new ViolatedAssumptionAnswer());
      doReturn(0, 0).when(intervalCategoryDataset0).getColumnCount();
      doReturn(0, 0).when(intervalCategoryDataset0).getRowCount();
      ChartFactory.createGanttChart("", "", "SE h!fb4L", intervalCategoryDataset0, false, false, true);
      BoxAndWhiskerXYDataset boxAndWhiskerXYDataset0 = mock(BoxAndWhiskerXYDataset.class, new ViolatedAssumptionAnswer());
      doReturn(0, 0, 0, 0, 0).when(boxAndWhiskerXYDataset0).getSeriesCount();
      ChartFactory.createBoxAndWhiskerChart("SE h!fb4L", "", "", boxAndWhiskerXYDataset0, true);
      XYDataset xYDataset0 = mock(XYDataset.class, new ViolatedAssumptionAnswer());
      doReturn(0).when(xYDataset0).getSeriesCount();
      PlotOrientation plotOrientation0 = mock(PlotOrientation.class, new ViolatedAssumptionAnswer());
      boolean boolean0 = true;
      // Undeclared exception!
      ChartFactory.createScatterPlot("", "SE h!fb4L", "", xYDataset0, plotOrientation0, true, true, true);
  }
}
