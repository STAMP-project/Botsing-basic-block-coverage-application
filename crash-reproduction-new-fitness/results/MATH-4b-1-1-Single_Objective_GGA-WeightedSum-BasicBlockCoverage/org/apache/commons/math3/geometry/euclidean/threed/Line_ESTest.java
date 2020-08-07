/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:28:32 UTC 2020
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
      Vector3D vector3D0 = Vector3D.NaN;
      Line line0 = new Line(vector3D0, vector3D0);
      Vector3D vector3D1 = vector3D0.negate();
      line0.getAbscissa(vector3D0);
      line0.getAbscissa(vector3D1);
      line0.wholeLine();
      Vector1D vector1D0 = line0.toSubSpace(vector3D1);
      Vector3D vector3D2 = line0.toSpace(vector1D0);
      line0.toSubSpace(vector3D2);
      Line line1 = new Line(line0);
      line0.closestPoint(line1);
      Line line2 = new Line(vector3D0, vector3D1);
      // Undeclared exception!
      line2.getAbscissa((Vector3D) null);
  }
}
