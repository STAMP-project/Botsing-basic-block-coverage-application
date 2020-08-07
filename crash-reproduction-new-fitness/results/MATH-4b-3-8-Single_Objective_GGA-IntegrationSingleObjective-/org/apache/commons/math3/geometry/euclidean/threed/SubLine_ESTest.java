/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:10:46 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.threed.Line;
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
      Vector3D vector3D1 = Vector3D.MINUS_I;
      vector3D1.getNorm1();
      Vector3D vector3D2 = Vector3D.crossProduct(vector3D1, vector3D0);
      Line line0 = new Line(vector3D1, vector3D2);
      Segment segment0 = new Segment(vector3D1, vector3D0, line0);
      vector3D1.toString();
      SubLine subLine0 = new SubLine(segment0);
      subLine0.getSegments();
      subLine0.getSegments();
      SubLine subLine1 = new SubLine(segment0);
      // Undeclared exception!
      subLine0.intersection(subLine1, false);
  }
}
