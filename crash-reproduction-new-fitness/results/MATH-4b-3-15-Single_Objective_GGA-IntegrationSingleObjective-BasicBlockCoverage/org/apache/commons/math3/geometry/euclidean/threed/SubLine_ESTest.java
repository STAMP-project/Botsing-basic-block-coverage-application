/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:08:15 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet;
import org.apache.commons.math3.geometry.euclidean.threed.Line;
import org.apache.commons.math3.geometry.euclidean.threed.Plane;
import org.apache.commons.math3.geometry.euclidean.threed.SubLine;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SubLine_ESTest extends SubLine_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Line line0 = mock(Line.class, new ViolatedAssumptionAnswer());
      IntervalsSet intervalsSet0 = mock(IntervalsSet.class, new ViolatedAssumptionAnswer());
      Vector3D vector3D0 = Vector3D.NaN;
      SubLine subLine0 = new SubLine(vector3D0, vector3D0);
      Plane plane0 = new Plane(vector3D0, vector3D0, vector3D0);
      Plane plane1 = new Plane(vector3D0, vector3D0);
      Line line1 = plane0.intersection(plane1);
      SubLine subLine1 = line1.wholeLine();
      // Undeclared exception!
      subLine0.intersection(subLine1, true);
  }
}
