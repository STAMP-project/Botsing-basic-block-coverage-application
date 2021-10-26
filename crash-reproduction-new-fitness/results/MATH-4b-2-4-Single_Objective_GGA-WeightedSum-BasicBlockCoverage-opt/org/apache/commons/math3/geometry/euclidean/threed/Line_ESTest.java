/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:19:15 UTC 2021
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
      Line line0 = new Line(vector3D0, vector3D0);
      line0.getAbscissa(vector3D0);
      line0.wholeLine();
      Vector1D vector1D0 = Vector1D.NEGATIVE_INFINITY;
      Vector3D vector3D1 = line0.toSpace(vector1D0);
      line0.distance(line0);
      line0.isSimilarTo(line0);
      line0.wholeLine();
      line0.toSubSpace(vector3D1);
      line0.isSimilarTo(line0);
      Line line1 = new Line(line0);
      line1.distance(vector3D0);
      line1.intersection(line0);
      line0.distance(vector3D0);
      // Undeclared exception!
      line1.toSubSpace((Vector<Euclidean3D>) null);
  }
}
