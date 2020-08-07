/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:35:10 UTC 2020
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
      Vector3D vector3D0 = new Vector3D(58.34517069256649, 58.34517069256649, 58.34517069256649);
      Vector3D vector3D1 = Vector3D.MINUS_K;
      Vector3D vector3D2 = new Vector3D(58.34517069256649, 0.0, 0.0);
      Vector3D vector3D3 = vector3D0.normalize();
      Vector3D vector3D4 = new Vector3D(0.0, vector3D1);
      Line line0 = new Line(vector3D3, vector3D2);
      Line line1 = new Line(line0);
      line1.getAbscissa(vector3D4);
      // Undeclared exception!
      line0.toSubSpace((Vector<Euclidean3D>) null);
  }
}
