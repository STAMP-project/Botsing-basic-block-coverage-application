/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:04:57 UTC 2020
 */

package org.jfree.chart.plot;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.jfree.chart.axis.CyclicNumberAxis;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.WindItemRenderer;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.xy.DefaultIntervalXYDataset;
import org.jfree.data.xy.DefaultXYZDataset;
import org.jfree.data.xy.OHLCDataItem;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DefaultXYZDataset defaultXYZDataset0 = new DefaultXYZDataset();
      TimeZone timeZone0 = TimeZone.getDefault();
      GregorianCalendar gregorianCalendar0 = new GregorianCalendar(timeZone0);
      Date date0 = gregorianCalendar0.getGregorianChange();
      gregorianCalendar0.getGregorianChange();
      OHLCDataItem oHLCDataItem0 = new OHLCDataItem(date0, 75.0, 75.0, (-3726.425718202908), 50.0, 265.5498714806047);
      WindItemRenderer windItemRenderer0 = new WindItemRenderer();
      DefaultIntervalXYDataset defaultIntervalXYDataset0 = new DefaultIntervalXYDataset();
      CyclicNumberAxis cyclicNumberAxis0 = new CyclicNumberAxis(265.5498714806047, (-787.25079548274));
      XYPlot xYPlot0 = new XYPlot(defaultXYZDataset0, cyclicNumberAxis0, cyclicNumberAxis0, (XYItemRenderer) null);
  }
}
