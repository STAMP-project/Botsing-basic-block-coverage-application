/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:07:28 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.Vector;
import org.apache.commons.math3.geometry.euclidean.oned.Vector1D;
import org.apache.commons.math3.geometry.euclidean.threed.Euclidean3D;
import org.apache.commons.math3.geometry.euclidean.threed.Line;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Line_ESTest extends Line_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Vector3D vector3D0 = mock(Vector3D.class, new ViolatedAssumptionAnswer());
      doReturn((Vector3D) null).when(vector3D0).subtract(nullable(org.apache.commons.math3.geometry.Vector.class));
      Vector3D vector3D1 = Vector3D.MINUS_J;
      Vector1D vector1D0 = Vector1D.POSITIVE_INFINITY;
      Vector3D vector3D2 = Vector3D.POSITIVE_INFINITY;
      Vector3D vector3D3 = vector3D2.crossProduct((Vector<Euclidean3D>) vector3D1);
      Line line0 = new Line(vector3D1, vector3D2);
      vector1D0.negate();
      Vector3D vector3D4 = Vector3D.NaN;
      line0.getAbscissa(vector3D4);
      line0.toSpace(vector1D0);
      line0.wholeLine();
      Vector3D vector3D5 = line0.getOrigin();
      line0.distance(vector3D5);
      Line line1 = new Line(line0);
      line1.wholeLine();
      line1.distance(vector3D3);
      line0.isSimilarTo(line1);
      Vector3D vector3D6 = line1.pointAt(Double.NaN);
      Line line2 = new Line(vector3D4, vector3D5);
      Vector3D vector3D7 = new Vector3D(641.17029, vector3D6, (-1.7168146928204135), vector3D2);
      Line line3 = new Line(vector3D1, vector3D3);
      Vector3D vector3D8 = Vector3D.NEGATIVE_INFINITY;
      line2.toSubSpace(vector3D4);
      line2.getOrigin();
      Line line4 = new Line(line3);
      // Undeclared exception!
      line4.getAbscissa(vector3D0);
  }
}
