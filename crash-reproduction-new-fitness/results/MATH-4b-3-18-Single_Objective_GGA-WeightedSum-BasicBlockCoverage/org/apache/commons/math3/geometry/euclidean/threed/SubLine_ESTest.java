/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 00:55:25 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.threed.Line;
import org.apache.commons.math3.geometry.euclidean.threed.Plane;
import org.apache.commons.math3.geometry.euclidean.threed.Segment;
import org.apache.commons.math3.geometry.euclidean.threed.SubLine;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SubLine_ESTest extends SubLine_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Vector3D vector3D0 = Vector3D.NEGATIVE_INFINITY;
      Plane plane0 = new Plane(vector3D0, vector3D0);
      Plane plane1 = new Plane(vector3D0);
      Line line0 = plane0.intersection(plane1);
      Line line1 = new Line(line0);
      Segment segment0 = new Segment(vector3D0, vector3D0, line1);
      SubLine subLine0 = new SubLine(segment0);
      SubLine subLine1 = line0.wholeLine();
      // Undeclared exception!
      subLine1.intersection(subLine0, true);
  }
}
