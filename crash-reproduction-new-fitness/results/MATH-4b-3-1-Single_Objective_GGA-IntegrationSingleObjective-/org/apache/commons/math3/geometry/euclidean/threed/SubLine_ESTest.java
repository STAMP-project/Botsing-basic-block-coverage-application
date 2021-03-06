/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:10:13 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet;
import org.apache.commons.math3.geometry.euclidean.threed.Line;
import org.apache.commons.math3.geometry.euclidean.threed.RotationOrder;
import org.apache.commons.math3.geometry.euclidean.threed.Segment;
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
      Vector3D vector3D0 = Vector3D.NEGATIVE_INFINITY;
      RotationOrder rotationOrder0 = RotationOrder.XYZ;
      Vector3D vector3D1 = rotationOrder0.getA1();
      Line line1 = new Line(vector3D0, vector3D1);
      SubLine subLine0 = line1.wholeLine();
      Segment segment0 = new Segment(vector3D1, vector3D0, line1);
      // Undeclared exception!
      subLine0.intersection(subLine0, false);
  }
}
