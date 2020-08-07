/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 00:56:30 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.Vector;
import org.apache.commons.math3.geometry.euclidean.threed.Euclidean3D;
import org.apache.commons.math3.geometry.euclidean.threed.Line;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Line_ESTest extends Line_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Vector3D vector3D0 = Vector3D.PLUS_J;
      Vector3D vector3D1 = new Vector3D(0.0, vector3D0, (-1504.9199), vector3D0);
      Line line0 = new Line(vector3D1, vector3D0);
      Line line1 = new Line(line0);
      line1.pointAt(1642.0741);
      Line line2 = new Line(line0);
      double double0 = 260.7;
      Vector3D vector3D2 = new Vector3D((-1504.9199), (-1504.9199), 260.7);
      // Undeclared exception!
      line2.toSubSpace((Vector<Euclidean3D>) null);
  }
}
