/*
 * This file was automatically generated by EvoSuite
 * Mon Nov 01 15:11:53 UTC 2021
 */

package org.jfree.chart;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartTheme;
import org.jfree.chart.StandardChartTheme;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.ThermometerPlot;
import org.jfree.data.time.TimePeriodValues;
import org.jfree.data.time.TimePeriodValuesCollection;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class ChartFactory_ESTest extends ChartFactory_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ChartTheme chartTheme0 = StandardChartTheme.createLegacyTheme();
      ChartFactory.setChartTheme(chartTheme0);
      TimePeriodValues timePeriodValues0 = new TimePeriodValues("emDXFp");
      TimePeriodValues timePeriodValues1 = timePeriodValues0.createCopy(0, 0);
      TimePeriodValuesCollection timePeriodValuesCollection0 = new TimePeriodValuesCollection(timePeriodValues1);
      ThermometerPlot thermometerPlot0 = new ThermometerPlot();
      PlotOrientation plotOrientation0 = thermometerPlot0.getOrientation();
      // Undeclared exception!
      ChartFactory.createScatterPlot("`WKo", "`WKo", "emDXFp", timePeriodValuesCollection0, plotOrientation0, true, true, true);
  }
}
