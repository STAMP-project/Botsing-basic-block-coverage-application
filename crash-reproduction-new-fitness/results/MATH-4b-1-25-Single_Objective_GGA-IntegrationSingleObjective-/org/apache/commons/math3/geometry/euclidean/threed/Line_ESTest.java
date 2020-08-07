/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:34:53 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
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
      Line line0 = new Line(vector3D0, vector3D0);
      Line line1 = new Line(line0);
      line1.getAbscissa(vector3D0);
      Vector3D vector3D1 = line0.closestPoint(line1);
      line1.toSubSpace(vector3D0);
      Vector3D vector3D2 = line1.getDirection();
      Line line2 = new Line(line0);
      line1.getAbscissa(vector3D0);
      Line line3 = new Line(vector3D2, vector3D1);
      line3.toSubSpace(vector3D0);
      line1.isSimilarTo(line1);
      Line line4 = new Line(vector3D0, vector3D1);
      line3.contains(vector3D1);
      Line line5 = new Line(line1);
      line0.distance(line3);
      line5.wholeLine();
      // Undeclared exception!
      line5.getAbscissa((Vector3D) null);
  }
}
