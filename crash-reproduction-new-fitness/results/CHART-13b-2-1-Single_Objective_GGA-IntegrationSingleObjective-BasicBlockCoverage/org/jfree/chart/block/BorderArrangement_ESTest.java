/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:09:05 UTC 2020
 */

package org.jfree.chart.block;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.awt.Graphics2D;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.block.BlockContainer;
import org.jfree.chart.block.BorderArrangement;
import org.jfree.chart.block.RectangleConstraint;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.title.LegendTitle;
import org.jfree.data.Range;
import org.jfree.data.time.DateRange;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BorderArrangement_ESTest extends BorderArrangement_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BorderArrangement borderArrangement0 = new BorderArrangement();
      BlockContainer blockContainer0 = mock(BlockContainer.class, new ViolatedAssumptionAnswer());
      DateRange dateRange0 = DateAxis.DEFAULT_DATE_RANGE;
      BlockContainer blockContainer1 = mock(BlockContainer.class, new ViolatedAssumptionAnswer());
      Graphics2D graphics2D0 = null;
      RectangleConstraint rectangleConstraint0 = mock(RectangleConstraint.class, new ViolatedAssumptionAnswer());
      XYPlot xYPlot0 = new XYPlot();
      JFreeChart jFreeChart0 = new JFreeChart(xYPlot0);
      LegendTitle legendTitle0 = jFreeChart0.getLegend(0);
      legendTitle0.getItemContainer();
      double double0 = (-95.185);
      double double1 = (-104.49);
      Range range0 = new Range(0, (-104.49));
  }
}
