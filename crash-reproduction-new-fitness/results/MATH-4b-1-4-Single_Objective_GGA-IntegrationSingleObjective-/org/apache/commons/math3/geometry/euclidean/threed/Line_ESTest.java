/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:10:29 UTC 2020
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
      Vector3D vector3D1 = Vector3D.NaN;
      line0.distance(vector3D0);
      line0.reset(vector3D0, vector3D0);
      Line line1 = new Line(vector3D0, vector3D1);
      line1.distance(vector3D1);
      line1.intersection(line0);
      // Undeclared exception!
      line1.getAbscissa((Vector3D) null);
  }
}
