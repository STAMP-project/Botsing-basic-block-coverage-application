/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:19:17 UTC 2021
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
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
      Vector3D vector3D0 = Vector3D.NEGATIVE_INFINITY;
      Line line0 = new Line(vector3D0, vector3D0);
      line0.reset(vector3D0, vector3D0);
      line0.wholeLine();
      Vector3D vector3D1 = line0.getDirection();
      line0.toSubSpace(vector3D1);
      Vector3D vector3D2 = Vector3D.NaN;
      line0.distance(vector3D2);
      line0.getDirection();
      line0.intersection(line0);
      Vector3D vector3D3 = Vector3D.MINUS_I;
      Locale locale0 = Locale.JAPAN;
      Line line1 = new Line(line0);
      line0.isSimilarTo(line1);
      line0.contains(vector3D2);
      // Undeclared exception!
      line0.toSubSpace((Vector<Euclidean3D>) null);
  }
}
