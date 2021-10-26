/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:19:36 UTC 2021
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
      Vector3D vector3D0 = Vector3D.NaN;
      Vector3D vector3D1 = Vector3D.MINUS_K;
      Line line0 = new Line(vector3D1, vector3D0);
      Vector3D vector3D2 = Vector3D.ZERO;
      vector3D2.dotProduct((Vector<Euclidean3D>) vector3D0);
      vector3D0.dotProduct((Vector<Euclidean3D>) vector3D2);
      Line line1 = new Line(vector3D2, vector3D1);
      line1.closestPoint(line0);
      line0.isSimilarTo(line1);
      Line line2 = new Line(line0);
      Vector3D vector3D3 = line2.pointAt(0.0);
      line2.distance(line0);
      Line line3 = line0.revert();
      vector3D3.negate();
      line2.intersection(line0);
      line2.wholeLine();
      line2.wholeLine();
      line3.getOrigin();
      line3.reset(vector3D3, vector3D0);
      line2.isSimilarTo(line3);
      // Undeclared exception!
      line0.toSubSpace((Vector<Euclidean3D>) null);
  }
}
