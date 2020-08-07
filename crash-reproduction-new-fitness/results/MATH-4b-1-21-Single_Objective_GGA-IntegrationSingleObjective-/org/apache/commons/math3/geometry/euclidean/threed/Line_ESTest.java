/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:34:30 UTC 2020
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
      Vector3D vector3D0 = Vector3D.POSITIVE_INFINITY;
      Line line0 = new Line(vector3D0, vector3D0);
      Line line1 = line0.revert();
      Vector3D vector3D1 = new Vector3D(1.698698203637178E-16, 1.698698203637178E-16, 1.698698203637178E-16);
      Line line2 = new Line(vector3D1, vector3D0);
      Line line3 = new Line(line2);
      Line line4 = new Line(line1);
      Line line5 = new Line(line4);
      line4.toSubSpace(vector3D0);
      // Undeclared exception!
      line5.getAbscissa((Vector3D) null);
  }
}
