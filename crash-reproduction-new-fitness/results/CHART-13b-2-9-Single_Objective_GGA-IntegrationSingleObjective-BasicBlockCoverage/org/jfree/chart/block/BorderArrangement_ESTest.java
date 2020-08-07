/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:11:23 UTC 2020
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
import org.jfree.chart.block.RectangleConstraint;
import org.jfree.data.Range;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BorderArrangement_ESTest extends BorderArrangement_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Graphics2D graphics2D0 = mock(Graphics2D.class, new ViolatedAssumptionAnswer());
      BorderArrangement borderArrangement0 = new BorderArrangement();
      BlockContainer blockContainer0 = new BlockContainer(borderArrangement0);
      RectangleConstraint rectangleConstraint0 = new RectangleConstraint((Range) null, (Range) null);
      borderArrangement0.arrangeFF(blockContainer0, graphics2D0, rectangleConstraint0);
      borderArrangement0.clear();
      float[] floatArray0 = new float[5];
      float float0 = 0.0F;
      floatArray0[0] = 0.0F;
      float float1 = 2384.0F;
      floatArray0[1] = 2384.0F;
      float float2 = (-0.34494045F);
      floatArray0[2] = (-0.34494045F);
      float float3 = 3253.9404F;
      floatArray0[3] = 3253.9404F;
      double double0 = (-2730.400830036169);
      Range range0 = new Range(504.81, (-2730.400830036169));
  }
}
