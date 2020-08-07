/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:11:24 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.Vector;
import org.apache.commons.math3.geometry.euclidean.oned.Vector1D;
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
      Vector3D vector3D0 = Vector3D.NEGATIVE_INFINITY;
      Vector3D vector3D1 = Vector3D.MINUS_I;
      Vector3D vector3D2 = vector3D0.subtract((Vector<Euclidean3D>) vector3D1);
      Line line0 = new Line(vector3D0, vector3D0);
      Line line1 = new Line(line0);
      Vector3D vector3D3 = new Vector3D(0.0, vector3D0, 0.0, vector3D0, (-279.33), vector3D0);
      Vector3D vector3D4 = Vector3D.MINUS_J;
      Vector1D vector1D0 = line1.toSubSpace(vector3D2);
      line1.getOrigin();
      Vector3D vector3D5 = line1.toSpace(vector1D0);
      line1.contains(vector3D4);
      Vector3D vector3D6 = new Vector3D(0.0, 6.08778341640915E9);
      Line line2 = new Line(vector3D6, vector3D3);
      Line line3 = new Line(vector3D5, vector3D6);
      Line line4 = new Line(line1);
      line4.reset(vector3D5, vector3D2);
      // Undeclared exception!
      line1.toSubSpace((Vector<Euclidean3D>) null);
  }
}
