/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:34:41 UTC 2020
 */

package org.apache.commons.math3.geometry.euclidean.threed;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.geometry.euclidean.threed.Line;
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
      Line line1 = mock(Line.class, new ViolatedAssumptionAnswer());
      Vector3D vector3D0 = Vector3D.NaN;
      Vector3D vector3D1 = Vector3D.MINUS_I;
      SubLine subLine0 = new SubLine(vector3D0, vector3D1);
      subLine0.getSegments();
      SubLine subLine1 = new SubLine(vector3D0, vector3D0);
      // Undeclared exception!
      subLine1.intersection(subLine0, true);
  }
}
