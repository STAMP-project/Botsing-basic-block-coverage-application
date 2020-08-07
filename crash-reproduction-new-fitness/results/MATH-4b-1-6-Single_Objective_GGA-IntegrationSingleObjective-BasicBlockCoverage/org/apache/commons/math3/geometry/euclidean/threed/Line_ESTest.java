/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:07:22 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.oned.Vector1D;
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
      Vector3D vector3D1 = Vector3D.crossProduct(vector3D0, vector3D0);
      Line line0 = new Line(vector3D1, vector3D0);
      line0.distance(vector3D0);
      line0.isSimilarTo(line0);
      Vector1D vector1D0 = Vector1D.NEGATIVE_INFINITY;
      Vector3D vector3D2 = line0.toSpace(vector1D0);
      line0.toSpace(vector1D0);
      Line line1 = new Line(line0);
      line1.isSimilarTo(line0);
      line1.reset(vector3D1, vector3D2);
      Line line2 = line1.revert();
      Vector3D vector3D3 = line2.getOrigin();
      line2.getAbscissa(vector3D3);
      line0.intersection(line1);
      line1.getAbscissa(vector3D0);
      Vector3D vector3D4 = Vector3D.PLUS_I;
      line2.distance(vector3D4);
      line2.wholeLine();
      line2.getAbscissa(vector3D0);
      line0.getDirection();
      Line line3 = new Line(line1);
      // Undeclared exception!
      line3.getAbscissa((Vector3D) null);
  }
}
