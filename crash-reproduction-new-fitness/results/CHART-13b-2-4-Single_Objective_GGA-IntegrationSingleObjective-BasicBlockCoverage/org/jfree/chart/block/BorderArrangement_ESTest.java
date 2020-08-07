/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:11:54 UTC 2020
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
import org.jfree.chart.block.BlockContainer;
import org.jfree.chart.block.BorderArrangement;
import org.jfree.chart.block.CenterArrangement;
import org.jfree.data.Range;
import org.jfree.data.xy.XIntervalSeriesCollection;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BorderArrangement_ESTest extends BorderArrangement_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BlockContainer blockContainer0 = mock(BlockContainer.class, new ViolatedAssumptionAnswer());
      CenterArrangement centerArrangement0 = new CenterArrangement();
      BlockContainer blockContainer1 = new BlockContainer();
      BlockContainer blockContainer2 = new BlockContainer();
      Graphics2D graphics2D0 = null;
      BorderArrangement borderArrangement0 = new BorderArrangement();
      BorderArrangement borderArrangement1 = new BorderArrangement();
      double double0 = 0.0;
      borderArrangement0.arrangeFN(blockContainer2, (Graphics2D) null, 0.0);
      XIntervalSeriesCollection xIntervalSeriesCollection0 = new XIntervalSeriesCollection();
      double double1 = (-755.7);
      Range range0 = new Range(0.0, (-755.7));
  }
}
