/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:19:34 UTC 2021
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.threed.Line;
import org.apache.commons.math3.geometry.euclidean.threed.Plane;
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
      boolean boolean0 = true;
      Vector3D vector3D0 = Vector3D.POSITIVE_INFINITY;
      Vector3D vector3D1 = Vector3D.PLUS_J;
      Plane plane0 = new Plane(vector3D0, vector3D0, vector3D1);
      Plane plane1 = new Plane(plane0);
      Line line1 = plane0.intersection(plane1);
      Line line2 = new Line(line1);
      plane0.intersection(line2);
      Vector3D vector3D2 = Plane.intersection(plane0, plane0, plane0);
      Vector3D vector3D3 = new Vector3D(3133.0, vector3D0, 3133.0, vector3D0, 888.99670807504, vector3D0, 3133.0, vector3D2);
      Plane plane2 = new Plane(vector3D3, vector3D2);
      Line line3 = plane2.intersection(plane0);
      Segment segment0 = new Segment(vector3D2, vector3D3, line3);
      SubLine subLine0 = new SubLine(segment0);
      SubLine subLine1 = line3.wholeLine();
      boolean boolean1 = true;
      // Undeclared exception!
      subLine0.intersection(subLine1, true);
  }
}
