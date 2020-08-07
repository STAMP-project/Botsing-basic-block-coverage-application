/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:28:54 UTC 2020
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
      Vector3D vector3D0 = Vector3D.POSITIVE_INFINITY;
      Vector3D vector3D1 = new Vector3D(0.0, vector3D0);
      Line line0 = new Line(vector3D0, vector3D1);
      Line line1 = new Line(line0);
      Vector3D vector3D2 = new Vector3D(0.0, vector3D1);
      Line line2 = new Line(vector3D1, vector3D2);
      Line line3 = new Line(line2);
      Line line4 = new Line(vector3D0, vector3D1);
      line4.toSubSpace(vector3D0);
      line0.getAbscissa(vector3D1);
      Vector3D vector3D3 = Vector3D.MINUS_J;
      Vector3D vector3D4 = Vector3D.MINUS_I;
      Line line5 = new Line(vector3D4, vector3D2);
      line5.contains(vector3D0);
      Line line6 = new Line(vector3D3, vector3D0);
      Vector1D vector1D0 = Vector1D.POSITIVE_INFINITY;
      Vector1D vector1D1 = Vector1D.ZERO;
      line2.toSpace(vector1D1);
      Line line7 = new Line(vector3D0, vector3D4);
      line5.wholeLine();
      Vector3D vector3D5 = line2.getOrigin();
      line7.wholeLine();
      Line line8 = new Line(vector3D5, vector3D5);
      line8.getAbscissa(vector3D5);
      line8.distance(vector3D4);
      line6.closestPoint(line4);
      Vector3D vector3D6 = new Vector3D(Double.NaN, vector3D1);
      line1.toSubSpace(vector3D6);
      line7.intersection(line6);
      // Undeclared exception!
      line3.toSubSpace((Vector<Euclidean3D>) null);
  }
}
