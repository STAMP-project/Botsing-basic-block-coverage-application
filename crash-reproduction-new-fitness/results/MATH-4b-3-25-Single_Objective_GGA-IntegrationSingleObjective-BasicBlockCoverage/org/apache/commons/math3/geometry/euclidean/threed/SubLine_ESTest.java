/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:30:59 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.threed.Plane;
import org.apache.commons.math3.geometry.euclidean.threed.SubLine;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SubLine_ESTest extends SubLine_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Vector3D vector3D0 = Vector3D.NEGATIVE_INFINITY;
      Plane plane0 = new Plane(vector3D0, vector3D0);
      Plane plane1 = new Plane(plane0);
      Vector3D vector3D1 = Plane.intersection(plane1, plane1, plane0);
      SubLine subLine0 = new SubLine(vector3D0, vector3D1);
      Vector3D vector3D2 = Vector3D.MINUS_I;
      SubLine subLine1 = new SubLine(vector3D0, vector3D2);
      // Undeclared exception!
      subLine0.intersection(subLine1, false);
  }
}
