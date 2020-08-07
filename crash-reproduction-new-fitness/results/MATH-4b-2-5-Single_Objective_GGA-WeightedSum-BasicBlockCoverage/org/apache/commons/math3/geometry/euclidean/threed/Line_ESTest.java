/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:28:43 UTC 2020
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
      Vector3D vector3D0 = Vector3D.POSITIVE_INFINITY;
      Vector3D vector3D1 = Vector3D.PLUS_I;
      Vector3D.angle(vector3D0, vector3D0);
      Vector3D vector3D2 = vector3D0.normalize();
      Line line0 = new Line(vector3D0, vector3D1);
      line0.getAbscissa(vector3D2);
      // Undeclared exception!
      line0.toSubSpace((Vector<Euclidean3D>) null);
  }
}
