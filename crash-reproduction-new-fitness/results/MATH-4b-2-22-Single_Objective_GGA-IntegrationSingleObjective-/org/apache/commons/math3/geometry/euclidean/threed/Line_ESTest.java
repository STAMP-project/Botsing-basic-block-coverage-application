/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:34:53 UTC 2020
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
      Vector3D vector3D0 = Vector3D.MINUS_I;
      Vector3D vector3D1 = Vector3D.POSITIVE_INFINITY;
      Vector3D vector3D2 = vector3D1.add((Vector<Euclidean3D>) vector3D0);
      Vector3D vector3D3 = Vector3D.NaN;
      Vector3D vector3D4 = vector3D0.crossProduct((Vector<Euclidean3D>) vector3D1);
      Line line0 = new Line(vector3D2, vector3D2);
      Line line1 = new Line(line0);
      Vector3D vector3D5 = line1.getDirection();
      Vector1D vector1D0 = line0.toSubSpace(vector3D5);
      line0.intersection(line1);
      Line line2 = new Line(vector3D0, vector3D5);
      Line line3 = new Line(vector3D5, vector3D0);
      Vector3D vector3D6 = Vector3D.MINUS_J;
      line0.toSubSpace(vector3D4);
      line2.intersection(line0);
      line1.toSpace(vector1D0);
      // Undeclared exception!
      line2.getAbscissa((Vector3D) null);
  }
}
