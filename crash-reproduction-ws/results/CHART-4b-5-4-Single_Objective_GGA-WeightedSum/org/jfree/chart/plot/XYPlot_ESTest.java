/*
 * This file was automatically generated by EvoSuite
 * Mon Mar 30 16:25:00 UTC 2020
 */

package org.jfree.chart.plot;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.lowagie.text.pdf.PdfName;
import java.util.Date;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.jfree.chart.axis.CyclicNumberAxis;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYItemRenderer;
import org.jfree.data.xy.DefaultOHLCDataset;
import org.jfree.data.xy.OHLCDataItem;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class XYPlot_ESTest extends XYPlot_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PdfName pdfName0 = PdfName.TRANSFORMMETHOD;
      OHLCDataItem[] oHLCDataItemArray0 = new OHLCDataItem[1];
      Date date0 = new Date();
      OHLCDataItem oHLCDataItem0 = new OHLCDataItem(date0, (-632.0), (-632.0), 0.0, 0.0, (-632.0));
      oHLCDataItemArray0[0] = oHLCDataItem0;
      DefaultOHLCDataset defaultOHLCDataset0 = new DefaultOHLCDataset(pdfName0, oHLCDataItemArray0);
      CyclicNumberAxis cyclicNumberAxis0 = new CyclicNumberAxis(0.0);
      XYPlot xYPlot0 = new XYPlot(defaultOHLCDataset0, cyclicNumberAxis0, cyclicNumberAxis0, (XYItemRenderer) null);
  }
}
