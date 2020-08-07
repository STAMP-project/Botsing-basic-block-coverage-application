/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:39:10 GMT+00:00 2020
 */

package org.jfree.chart.axis;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.awt.geom.Arc2D;
import java.text.NumberFormat;
import java.util.Locale;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.jfree.chart.axis.CyclicNumberAxis;
import org.jfree.chart.axis.NumberAxis3D;
import org.jfree.chart.labels.StandardXYToolTipGenerator;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.AbstractRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.chart.renderer.xy.XYStepAreaRenderer;
import org.jfree.chart.urls.StandardXYZURLGenerator;
import org.jfree.data.general.DefaultValueDataset;
import org.jfree.data.xy.DefaultXYZDataset;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Axis_ESTest extends Axis_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Double double0 = AbstractRenderer.ZERO;
      DefaultValueDataset defaultValueDataset0 = new DefaultValueDataset((Number) double0);
      CyclicNumberAxis cyclicNumberAxis0 = new CyclicNumberAxis((-696.301433780773));
      cyclicNumberAxis0.isTickLabelsVisible();
      DefaultXYZDataset defaultXYZDataset0 = new DefaultXYZDataset();
      Locale locale0 = Locale.ENGLISH;
      NumberFormat numberFormat0 = NumberFormat.getInstance(locale0);
      StandardXYToolTipGenerator standardXYToolTipGenerator0 = new StandardXYToolTipGenerator("producer", numberFormat0, numberFormat0);
      StandardXYZURLGenerator standardXYZURLGenerator0 = new StandardXYZURLGenerator();
      XYStepAreaRenderer xYStepAreaRenderer0 = new XYStepAreaRenderer((-1359), standardXYToolTipGenerator0, standardXYZURLGenerator0);
      NumberAxis3D numberAxis3D0 = new NumberAxis3D();
      Arc2D.Double arc2D_Double0 = new Arc2D.Double(0);
      numberAxis3D0.setUpArrow(arc2D_Double0);
      XYPlot xYPlot0 = new XYPlot(defaultXYZDataset0, numberAxis3D0, cyclicNumberAxis0, (XYItemRenderer) null);
  }
}
