/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:42:25 UTC 2020
 */

package org.jfree.chart.block;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.sql.Connection;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.block.BlockContainer;
import org.jfree.chart.block.BorderArrangement;
import org.jfree.chart.title.TextTitle;
import org.jfree.data.Range;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BorderArrangement_ESTest extends BorderArrangement_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BlockContainer blockContainer0 = mock(BlockContainer.class, new ViolatedAssumptionAnswer());
      String string0 = "SLSqvE";
      TextTitle textTitle0 = new TextTitle();
      BorderArrangement borderArrangement0 = new BorderArrangement();
      Connection connection0 = mock(Connection.class, new ViolatedAssumptionAnswer());
      Range range0 = ValueAxis.DEFAULT_RANGE;
      Range range1 = new Range(2.5E8, 1187.1456196806);
  }
}
