/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:28:53 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet;
import org.apache.commons.math3.geometry.euclidean.threed.Line;
import org.apache.commons.math3.geometry.euclidean.threed.SubLine;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SubLine_ESTest extends SubLine_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Line line0 = mock(Line.class, new ViolatedAssumptionAnswer());
      IntervalsSet intervalsSet0 = mock(IntervalsSet.class, new ViolatedAssumptionAnswer());
      SubLine subLine0 = new SubLine(line0, intervalsSet0);
      Vector3D vector3D0 = Vector3D.NEGATIVE_INFINITY;
      Vector3D vector3D1 = new Vector3D(0.6299605249474366, vector3D0, (-1402.012644028), vector3D0, 0.0, vector3D0, (-4058.0), vector3D0);
      Line line1 = new Line(vector3D1, vector3D1);
      line1.revert();
      IntervalsSet intervalsSet1 = new IntervalsSet();
      SubLine subLine1 = new SubLine(line1, intervalsSet1);
      boolean boolean0 = true;
      SubLine subLine2 = new SubLine(vector3D1, vector3D1);
      // Undeclared exception!
      subLine1.intersection(subLine2, true);
  }
}
