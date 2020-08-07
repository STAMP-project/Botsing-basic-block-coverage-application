/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:55:58 UTC 2020
 */

package org.jfree.chart.plot;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.lowagie.text.pdf.PdfName;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.axis.NumberAxis3D;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.labels.XYToolTipGenerator;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.chart.renderer.xy.XYStepRenderer;
import org.jfree.chart.urls.StandardXYZURLGenerator;
import org.jfree.data.statistics.SimpleHistogramDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      XYPlot xYPlot0 = new XYPlot();
      AxisLocation axisLocation0 = mock(AxisLocation.class, new ViolatedAssumptionAnswer());
      xYPlot0.setDomainAxisLocation(2286, axisLocation0);
      NumberAxis3D numberAxis3D0 = new NumberAxis3D();
      xYPlot0.getDataRange(numberAxis3D0);
      xYPlot0.getDataRange(numberAxis3D0);
      PdfName pdfName0 = PdfName.USEOUTLINES;
      SimpleHistogramDataset simpleHistogramDataset0 = new SimpleHistogramDataset(pdfName0);
      DateAxis dateAxis0 = new DateAxis();
      simpleHistogramDataset0.clone();
      StandardXYZURLGenerator standardXYZURLGenerator0 = new StandardXYZURLGenerator();
      XYStepRenderer xYStepRenderer0 = new XYStepRenderer((XYToolTipGenerator) null, standardXYZURLGenerator0);
      XYPlot xYPlot1 = new XYPlot(simpleHistogramDataset0, (ValueAxis) null, (ValueAxis) null, (XYItemRenderer) null);
      // Undeclared exception!
      xYPlot1.getDataRange((ValueAxis) null);
  }
}
