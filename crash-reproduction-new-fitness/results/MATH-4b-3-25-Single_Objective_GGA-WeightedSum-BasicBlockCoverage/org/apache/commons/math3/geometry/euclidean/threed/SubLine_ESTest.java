/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 00:56:20 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.threed.Line;
import org.apache.commons.math3.geometry.euclidean.threed.Segment;
import org.apache.commons.math3.geometry.euclidean.threed.SubLine;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SubLine_ESTest extends SubLine_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Vector3D vector3D0 = Vector3D.NaN;
      Vector3D vector3D1 = new Vector3D(793.673538269317, vector3D0, 793.673538269317, vector3D0, 793.673538269317, vector3D0, 0.0, vector3D0);
      Vector3D vector3D2 = Vector3D.POSITIVE_INFINITY;
      Line line0 = new Line(vector3D2, vector3D2);
      line0.getAbscissa(vector3D0);
      Segment segment0 = new Segment(vector3D1, vector3D0, line0);
      SubLine subLine0 = new SubLine(segment0);
      Vector3D vector3D3 = Vector3D.crossProduct(vector3D1, vector3D2);
      subLine0.getSegments();
      subLine0.getSegments();
      subLine0.getSegments();
      Segment segment1 = new Segment(vector3D2, vector3D3, line0);
      SubLine subLine1 = new SubLine(segment1);
      // Undeclared exception!
      subLine0.intersection(subLine1, true);
  }
}
